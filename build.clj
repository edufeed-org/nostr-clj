(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'net.clojars.laoc/nostr)
(def version (format "0.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def src-dirs ["src" "resources" "java"])

(defn test "Run all the tests." [opts]
  (let [basis    (b/create-basis {:aliases [:test]})
        cmds     (b/java-command
                  {:basis      basis
                    :main      'clojure.main
                    :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn compile-java "Compile Java source files." [opts]
  (b/javac {:src-dirs ["java"]            ;; Define the Java source directory
            :class-dir class-dir         ;; Output compiled classes to the target/classes directory
            :javac-opts ["--release" "11"]}) ;; Specify Java options
  opts)

(defn- pom-template [version]
  [[:description "FIXME: my new library."]
   [:url "https://github.com/sroertgen/nostr-clj"]
   [:licenses
    [:license
     [:name "Eclipse Public License"]
     [:url "http://www.eclipse.org/legal/epl-v10.html"]]]
   [:developers
    [:developer
     [:name "Laoc"]]]
   [:scm
    [:url "https://github.com/sroertgen/nostr-clj"]
    [:connection "scm:git:https://github.com/sroertgen/nostr-clj.git"]
    [:developerConnection "scm:git:ssh:git@github.com:sroertgen/nostr-clj.git"]
    [:tag (str "v" version)]]])

(defn- jar-opts [opts]
  (assoc opts
          :lib lib   :version version
          :jar-file  (format "target/%s-%s.jar" lib version)
          :basis     (b/create-basis {})
          :class-dir class-dir
          :target    "target"
          :src-dirs  src-dirs
          :pom-data  (pom-template version)))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (test opts)
  (b/delete {:path "target"})
  (let [opts (jar-opts opts)]
    (println "\nWriting pom.xml...")
    (b/write-pom opts)
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs src-dirs :target-dir class-dir})
    (println "\nBuilding JAR..." (:jar-file opts))
    (b/jar opts))
  opts)

(defn install "Install the JAR locally." [opts]
  (let [opts (jar-opts opts)]
    (compile-java opts)
    (b/install opts))
  opts)

(defn deploy "Deploy the JAR to Clojars." [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (compile-java opts)
    (dd/deploy {:installer :remote :artifact (b/resolve-path jar-file)
                :pom-file (b/pom-path (select-keys opts [:lib :class-dir]))}))
  opts)
