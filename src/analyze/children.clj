(ns analyze.children)
;; for code walking

(defmulti children :op)



;; TODO: nil vals 
(defmethod children :def [expr]
  [(:meta expr) (:init expr)])

(defmethod children :let [expr]
  (conj (:bindings expr)
        (:body expr)))

(defmethod children :letfn [expr]
  (conj (:binding-inits expr)
        (:body expr)))

(defmethod children :local-binding [expr]
  (when-let [init (:init expr)]
    [init]))

(defmethod children :binding-init [expr]
  [(:local-binding expr) (:init expr)])

;; TODO: ill-named?
(defmethod children :local-binding-expr [expr]
  [(:local-binding expr)])

(defmethod children :static-method [expr]
  (:args expr))

(defmethod children :instance-method [expr]
  (:args expr))

(defmethod children :static-field [expr]
  nil)

(defmethod children :instance-field [expr]
  [(:target expr)])

(defmethod children :new [expr]
  (:args expr))

(defmethod children :literal [expr]
  nil)

;; TODO: ill-named?
(defmethod children :empty-expr [expr]
  nil)

(defmethod children :set [expr]
  (:keys expr))

(defmethod children :vector [expr]
  (:args expr))

(defmethod children :map [expr]
  (:keyvals expr))

(defmethod children :monitor-enter [expr]
  [(:target expr)])

(defmethod children :monitor-exit [expr]
  [(:target expr)])

(defmethod children :throw [expr]
  [(:exception expr)])

(defmethod children :invoke [expr]
  (cons (:fexpr expr)
        (:args expr)))

(defmethod children :keyword-invoke [expr]
  [(:target expr)])

(defmethod children :var [expr]
  nil)

(defmethod children :the-var [expr]
  nil)

(defmethod children :unresolved-var [expr]
  nil)

;; TODO: ill-named?
(defmethod children :obj-expr [expr]
  nil)

(defmethod children :new-instance-method [expr]
  [(:body expr)])

(defmethod children :fn-method [expr]
  [(:body expr)])

;; TODO: ill-named?
(defmethod children :fn-expr [expr]
  (:methods expr))

;; TODO: ill-named?
(defmethod children :new-instance-expr [expr]
  (:methods children))

(defmethod children :instance-of [expr]
  [(:the-expr expr)])

(defmethod children :meta [expr]
  [(:meta expr) (:expr expr)])

(defmethod children :if [expr]
  [(:test expr) (:then expr) (:else expr)])

(defmethod children :do [expr]
  (:exprs expr))

(defmethod children :case* [expr]
  (concat [(:the-expr expr)]
          (:tests expr)
          (:thens expr)
          [(:default expr)]))

(defmethod children :import* [expr]
  nil)

(defmethod children :set! [expr]
  [(:target expr) (:val expr)])

(defmethod children :catch [expr]
  [(:local-binding expr) (:handler expr)])

(defmethod children :try [expr]
  (concat [(:try-expr expr)]
          (when-let [finally-expr (:finally-expr expr)]
            [finally-expr])
          (:catch-exprs expr)))

(defmethod children :recur [expr]
  (concat (:loop-locals expr)
          (:args expr)))

(defmethod children :method-param [expr]
  nil)



(defn traverse [expr]
  (println (:op expr))
  (doseq [cexpr (children expr)]
    (traverse cexpr)))
