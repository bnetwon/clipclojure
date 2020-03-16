{

           :user {
; :dependencies [[junegunn/inspector "0.2.0"][spyscope "0.1.6"][io.aviso/pretty "0.1.37"]]
; :injections [(require 'spyscope.core)]
                  :source-paths ["dev" "src" "test"]
                  :dependencies [[junegunn/inspector "0.2.0"][io.aviso/pretty "0.1.37"]]
                  :injections [(require 'inspector.core)(require 'clojure.repl)(require 'clojure.inspector)]
                  :plugins [[io.aviso/pretty "0.1.37"]]
                  :middleware [io.aviso.lein-pretty/inject]
                  :main ^{:skip-aot true} swing-clip.clipboard}

            :pretty {
                     :dependencies [[io.aviso/pretty "0.1.37"]]
                     :plugins [[io.aviso/pretty "0.1.37"]]
                     :middleware [io.aviso.lein-pretty/inject]
                     :main ^{:skip-aot true} swing-clip.clipboard}}
