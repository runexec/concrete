(ns concrete.core)

(defn get-symbols 
  "Retrieve both private and public symbols from ns.
   All symbols are sorted-by meta line"
  [ns-symbol]
  (->> ns-symbol ns-interns vals (sort-by #(-> % meta :line))))

(defn start-and-stops
  "Returns a list of start and stop points for parsing"
  [ns-symbol]
  (let [syms (get-symbols ns-symbol)
        -even? 0 ;;(-> syms count even?)
        n-lines (into [] (map #(-> % meta :line) syms))
        n-lines (if -even?
                  n-lines
                  (conj n-lines 0))]
    (loop [n-seq []
           nl n-lines]
      (if-not (seq nl)
        (->> (conj n-seq 0) flatten rest (partition-all 2))
        (let [_ (repeat 2 (first nl))]
          (recur               
           (conj n-seq _)
           (rest nl)))))))
      
(defn guess-src-path [& [ns]]
  (str "src/"
       (-> (or ns *ns*)
           str
           (clojure.string/split #"\.")
           (#(clojure.string/join "/" %))
           (.replace "-" "_")
           (str ".clj"))))

(defn ns-to-line-seq
  "Namespace to line-seq."
  [ns-symbol]
  (line-seq 
   (clojure.java.io/reader
    (guess-src-path ns-symbol))))

(defn line-seq-range
  "Returns content between the start of x and before start y lines"
  [-line-seq start-n end-n]
  (let [[s e] [start-n end-n]
        _ (if (= e 0) -line-seq (take (dec e) -line-seq))]
       (drop (dec s) _)))

(defn build-code-blocks
  "load-file on ns to build source blocks."
  [ns-symbol]
  (let [code-blocks (atom [])]
    (load-file (guess-src-path ns-symbol))
    ;; build blocks
    (let [ls (ns-to-line-seq ns-symbol)]
      (doseq [[s e] (start-and-stops ns-symbol)
              :let [_ (apply str
                             (interpose "\n" (line-seq-range ls s e)))
                    action (partial swap! code-blocks conj)]]
        (action {:body _}))
      (swap! code-blocks
             (fn [x] 
               (map #(assoc %1 :meta %2)
                    x
                    (map #(meta %) (get-symbols ns-symbol))))))))

(defmacro body [ns-symbol -symbol]
  `(let [blocks# (build-code-blocks '~ns-symbol)]
     (->> blocks# 
          (filter #(= '~-symbol (-> % :meta :name)))
          first
          :body
          clojure.string/split-lines)))

(defmacro body-print [ns-symbol -symbol]
  `(doseq [_# (body ~ns-symbol ~-symbol)]
     (println _#)))
