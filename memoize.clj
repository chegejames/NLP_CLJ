(defn mem[function]
  (let [pi (atom {'(-1 "*" "*") 1})
        bp (atom {})]
    (with-meta
      (fn [k u v]
        (let [args `(~k ~u ~v)]
          (or (second (find @pi args))
              (let [ret (function args)]
                (swap! pi assoc  args ret)
                (swap! bp assoc args v)
                ret))))
      {:pi pi :bp bp})))

