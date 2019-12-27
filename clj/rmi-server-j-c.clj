(ns swing-clip.rmi-server-j-c)
;;(defmacro foo [])
(def ctccl
  (let [cl (java.net.URLClassLoader.
            (into-array [(java.net.URL. "http://127.0.0.1:8080/class-server/")]))]
    (.println System/out (str "Setting context classloader to " cl))
    (.setContextClassLoader (Thread/currentThread) cl)
    cl))

;;
(def retnum (atom 100))
(def myss (reify
    mys.MyService
    (setInt [this in] (reset! retnum in))
     (voidMethod [this] (println @retnum))
      (intMethod [this] ((constantly @retnum)))))
;(let [ff (reify mys.MyService
;           (setInt [this in] (reset! retnum in))
;     (voidMethod [this] (println @retnum))
;      (intMethod [this] (@retnum)))]
;  (instance? mys.MyService ff)) 

(defn run-rmi-server-cl [& {:keys [host port ssf csf r-ssf r-csf] :or {port 1099}}]
  (let [cl (java.net.URLClassLoader.
            (into-array [(java.net.URL. "http://127.0.0.1:8080/class-server/")]))
        hndlr (proxy [java.lang.reflect.InvocationHandler] []
                (invoke [proxy method args]
                  (.println System/out (str "Invoking method '" method "' with args '" args "'"))))
        prxy-clss (java.rmi.server.RMIClassLoader/loadProxyClass
                   nil 
                   (into-array ["mys.MyService"])
                   cl)
        ;impl (.newInstance (.getConstructor prxy-clss (into-array [java.lang.reflect.InvocationHandler]))
        ;                   (into-array [hndlr]))
        stub (java.rmi.server.UnicastRemoteObject/exportObject myss 0 csf ssf)
        rmi-reg (if host
                  (do
                    (.println System/out (format "Connecting to RMI registry on host/port %s/%s with csf %s"
                                                 host port r-csf))
                    (java.rmi.registry.LocateRegistry/getRegistry host port r-csf))
                  (do
                    (.println System/out (format "Creating RMI registry on port %s with csf %s and ssf %s"
                                                 port r-csf r-ssf))
                    (java.rmi.registry.LocateRegistry/createRegistry port r-csf r-ssf)))]
    do (.rebind rmi-reg "mys.MyService" stub)
    (.println System/out (format "Registered %s" stub))
    (.println System/out (format "Waiting for incoming calls on RMI %s / service %s" rmi-reg stub)) { :prxy prxy-clss  :rmi-reg rmi-reg } ))
