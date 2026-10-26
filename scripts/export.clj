(ns export
  (:require
    [babashka.process :as process]
    [clojure.string :as str]))

(def out-dir "target/export")
(def out-file (str out-dir "/brickbreaker.html"))

(defn compile-release! []
  (process/shell "npx" "shadow-cljs" "release" "app"
                 "--config-merge" (pr-str {:output-dir (str out-dir "/js")})))

(defn inline-script [html js]
  (let [script-tag "<script src=\"/js/main.js\"></script>"]
    (assert (str/includes? html script-tag))
    (str/replace html script-tag
                 (str "<script>\n" (str/replace js "</script" "<\\/script") "\n</script>"))))

(defn -main [& _args]
  (compile-release!)
  (spit out-file (inline-script (slurp "public/index.html")
                                (slurp (str out-dir "/js/main.js"))))
  (println "Wrote" out-file))
