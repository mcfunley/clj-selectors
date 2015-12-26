(import java.lang.UnsupportedOperationException)
(import java.lang.RuntimeException)


(defn- unsupported [msg] (throw (UnsupportedOperationException. (str msg))))

(defn- parse-error [msg] (throw (RuntimeException. (str msg))))
