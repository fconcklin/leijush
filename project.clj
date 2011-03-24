;; required reading: https://github.com/technomancy/leiningen/commit/270cf1dec750d7a0ea6303651cb49e027c2c2af2
(defproject leijush "1.0.0-SNAPSHOT"
  :description "Clojush with lein"
  :url "https://github.com/fconcklin/leijush"
  :license {:name "Gnu General Public License (GPL)"
	    :url "http://www.gnu.org/licenses/gpl.html"
	    :distribution :repo
	    :comments "same as Clojush"}
  :mailing-list {:name "push mailing list"
		 :archive "https://lists.hampshire.edu/pipermail/push/"
		 :post "push@lists.hampshire.edu"
		 :subscribe "https://lists.hampshire.edu/mailman/listinfo/push"
		 :unsubscribe "https://lists.hampshire.edu/mailman/listinfo/push"}
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :min-lein-version "1.3.0"
  :main leijush.core
  :aot [leijush.core])			; fix this? 


;; build in autodoc like here: https://github.com/weavejester/compojure/blob/master/project.clj
