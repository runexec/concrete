# Concrete [![endorse](https://api.coderwall.com/runexec/endorsecount.png)](https://coderwall.com/runexec)

Retrieve the source body of a binding

```clojure
user> (use 'concrete.core)
nil
user> (body-print concrete.core body-print)
(defmacro body-print [ns-symbol -symbol]
  `(doseq [_# (body ~ns-symbol ~-symbol)]
     (println _#)))
nil
user> (fn? build-code-blocks)
true
user> (doseq [_ (body concrete.core build-code-blocks)] (println _))
(defn build-code-blocks
  "load-file on ns to build source blocks."
  [ns-symbol]
  (load-file (guess-src-path ns-symbol))
  (let [ls (ns-to-line-seq ns-symbol)
        code-blocks (atom [])]
    (doseq [[s e] (start-and-stops ns-symbol)
            :let [_ (apply str
                           (interpose "\n" (line-seq-range ls s e)))
                  action (partial swap! code-blocks conj)]]
      (action {:body _}))
    (swap! code-blocks
           (fn [x] 
             (map #(assoc %1 :meta %2)
                  x
                  (map #(meta %) (get-symbols ns-symbol)))))))
nil
```

# Install

```bash
git clone https://github.com/runexec/concrete.git
cd concrete; lein install
```

Lein deps

```clojure
[concrete/concrete "0.1.0-SNAPSHOT"]
```

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
