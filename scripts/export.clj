(ns export
  (:require
    [babashka.fs :as fs]
    [babashka.process :as process]
    [clojure.string :as str]))

(def out-dir "target/export")
(def out-file (str out-dir "/brickbreaker.html"))
(def rcade-out-file "dist/index.html")

(defn compile-release! []
  (process/shell "npx" "shadow-cljs" "release" "app"
                 "--config-merge" (pr-str {:output-dir (str out-dir "/js")})))

(defn inline-script [html js]
  (let [script-tag "<script src=\"/js/main.js\"></script>"]
    (assert (str/includes? html script-tag))
    (str/replace html script-tag
                 (str "<script>\n" (str/replace js "</script" "<\\/script") "\n</script>"))))

(defn export! [file]
  (compile-release!)
  (fs/create-dirs (fs/parent file))
  (spit file (inline-script (slurp "public/index.html")
                            (slurp (str out-dir "/js/main.js"))))
  (println "Wrote" file))

(defn -main [& _args]
  (export! out-file))

(defn -main-rcade [& _args]
  (export! rcade-out-file))
