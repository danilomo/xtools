(ns danilomo.ls
  (:require [clojure.string :as str]
            [table.core :as t]
            [clojure.tools.cli :refer [parse-opts]])
  (:import [java.nio.file Files Paths LinkOption]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]
           [java.nio.file.attribute PosixFilePermission]))

(def EMPTY_STRINGS (into-array String []))

(def EMPTY_LINK_OPTIONS (into-array LinkOption []))

(defn format-file-time [file-time]
  (let [instant   (.toInstant file-time)
        formatter (.. DateTimeFormatter
                      (ofPattern "yyyy-MM-dd HH:mm")
                      (withZone (ZoneId/systemDefault)))]
    (.format formatter instant)))

(defn get-ls-style-permissions [path]
  (let [permissions (Files/getPosixFilePermissions path EMPTY_LINK_OPTIONS)
        permission-chars {:owner-read PosixFilePermission/OWNER_READ
                          :owner-write PosixFilePermission/OWNER_WRITE
                          :owner-execute PosixFilePermission/OWNER_EXECUTE
                          :group-read PosixFilePermission/GROUP_READ
                          :group-write PosixFilePermission/GROUP_WRITE
                          :group-execute PosixFilePermission/GROUP_EXECUTE
                          :others-read PosixFilePermission/OTHERS_READ
                          :others-write PosixFilePermission/OTHERS_WRITE
                          :others-execute PosixFilePermission/OTHERS_EXECUTE}
        file-type (cond
                    (Files/isDirectory path EMPTY_LINK_OPTIONS) "d"
                    (Files/isSymbolicLink path) "l"
                    :else "-")
        perm-groups [[:owner-read :owner-write :owner-execute]
                     [:group-read :group-write :group-execute]
                     [:others-read :others-write :others-execute]]
        rwx ["r" "w" "x"]]
    (apply str
           (concat
            [file-type]
            (for [group perm-groups
                  [perm char] (map vector group rwx)]
              (if (.contains permissions (permission-chars perm))
                char
                "-"))))))

(def ^:dynamic show-all false)

(defn list-dir [path]
  (->> (.iterator (Files/list path))
       iterator-seq
       (remove #(and (not show-all) (Files/isHidden %)))))

(defn str2path [path-str]
  (Paths/get path-str EMPTY_STRINGS))

(defn parse-paths [& paths]
  (map str2path paths))

(defn path-type [path]
  (let [exists  (Files/exists path EMPTY_LINK_OPTIONS)
        is-dir (and exists (Files/isDirectory path EMPTY_LINK_OPTIONS))]
    (cond is-dir :dir
          exists :file
          :else :not-existing)))

(defn detailed-properties [path]
  [(get-ls-style-permissions path)
   (Files/getOwner path  EMPTY_LINK_OPTIONS)
   (Files/getOwner path  EMPTY_LINK_OPTIONS) ; Java doesn't have means to get the user group
   (Files/size path)
   (format-file-time (Files/getLastModifiedTime path EMPTY_LINK_OPTIONS))
   (.toString path)])

(defn list-files-plain [paths]
  (str/join "\n" (map #(.toString %) paths)))

(defn list-files-detailed [paths]
  (let [entries (map detailed-properties paths)]
    (t/table-str entries  {:skip-header true :style (t/with-separator "  ")})))

(defn list-not-existing [paths]
  (str/join "\n" (map #(str "File " (.toString %) " does not exist.") paths)))

(def ^:dynamic  list-files list-files-plain)

(defn list-dirs [dirs multiple-folders]
  (let [print-fn (if multiple-folders
                   #(str (.toString %) ": \n" (list-files (list-dir %)))
                   #(list-files (list-dir %)))]
    (str/join "\n" (map print-fn dirs))))

(defn ls-str [input]
  (let [paths (apply parse-paths input)
        grouped-entries (group-by
                         #(get % 0)
                         (map (fn [p] [(path-type p) p]) paths))
        dirs (map second (:dir grouped-entries))
        not-existing (map second (:not-existing grouped-entries))
        files (map second (:file grouped-entries))
        multiple-folders (boolean (or (> (count dirs) 1) (and (seq files) (seq dirs))))]
    (str/join "\n" (remove (some-fn nil? str/blank?) [(list-not-existing not-existing)
                                                      (list-files files)
                                                      (list-dirs dirs multiple-folders)]))))

(defn ls [input]
  (println (ls-str input)))

(def cli-options
  [["-a" "--all"]
   ["-l" "--long"]])


(defn -main
  [& args]
  (let [args (parse-opts args cli-options)
        cli-args (:arguments args)
        {all :all long-format :long} (:options args)
        inputs (or (seq cli-args) ["."])]
    (binding
        [list-files (if long-format
                   list-files-detailed
                   list-files-plain)
         show-all all]
      (ls inputs))))

