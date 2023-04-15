(require '[clojure.string :as str])
(declare cm)

(defn eval-string [s]
   (when-some [code (not-empty (str/trim s))]
     (try {:result (js/scittle.core.eval_string code)}
          (catch js/Error e
            {:error (str (.-message e))}))))

(defn eval-me []
  (js/scittle.core.eval_string (-> cm .-state .-doc .toString)))

(defonce !viewer (atom ""))
(defonce last-result (atom ""))
(defonce eval-tail (atom nil))

(defn update-editor! [text cursor-pos]
  (let [end (count (some-> @!viewer .-state .-doc str))]
    (.dispatch @!viewer #js{:changes #js{:from 0 :to end :insert text}
                            :selection #js{:anchor cursor-pos :head cursor-pos}})))

(defn guard [x f] (when (f x) x))

(defn range-str [viewer selection]
  (str (.slice (.-doc .-state viewer)
               (.-from selection)
               (.-to selection))))

(defn wrap-key
  "Returns `k` or, if it is a keyword, its name."
  [k]
  (cond-> k
    (keyword? k) (name)))

(defn ^boolean in?* [k* obj]
  (js-in k* obj))

(defn ^boolean contains?* [obj k*]
  (and (some? obj)
       (in?* k* obj)))

(defn ^boolean js-contains?
  "Returns true if `obj` contains `k`.
  ```
  (contains? o :k)
  (contains? o .-k)
  ```"
  [obj k]
  (contains?* obj (wrap-key k)))

(defn main-selection [state]
  (let [obj57473 
        (let [obj57471 (.asSingle (.-selection state))
              k57472   "ranges"]
          (if (js-contains? obj57471 k57472) 
            (unchecked-get obj57471 k57472) js/undefined))
        k57474 0]
   (if (js-contains? obj57473 k57474) 
     (unchecked-get obj57473 k57474) js/undefined)))

(defn ^boolean top-type? [node-type] (.-isTop ^js node-type))
(defn top? [node] (top-type? (type node)))

(defn ^number end [^js node]
  {:pre [(.-to node)]}
  (.-to node))

(defn ^number start [^js node]
  {:pre [(.-from node)]}
  (.-from node))

;; a more zipper-like interface
(defn ^js up [node] (.-parent ^js node))

(defn uppermost-edge-here
  "Returns node or its highest ancestor that starts or ends at the cursor position."
  [pos node]
  (or (->> (iterate up node)
           (take-while (every-pred (complement top?)
                                   #(or (= pos (end %) (end node))
                                        (= pos (start %) (start node)))))
           (last))
      node))

(defn ^js tree
  "Returns a (Tree https://lezer.codemirror.net/docs/ref/#common.Tree) for editor state
  or the SyntaxNode at pos.
  If pos is given and we're using Clojure language support embedded in other languages (e.g. markdown)
  enters overlaid Clojure nodes (https://lezer.codemirror.net/docs/ref/#common.MountedTree)."
  ([^js state] (language/syntaxTree state))
  ([^js state pos] (-> state language/syntaxTree (.resolveInner pos)))
  ([^js state pos dir] (-> state language/syntaxTree (.resolveInner pos dir))))

(defn nearest-touching [^js state pos dir]
  (let [L (some-> (tree state pos -1)
                  (u/guard (j/fn [^:js {:keys [to]}] (= pos to))))
        R (some-> (tree state pos 1)
                  (u/guard (j/fn [^:js {:keys [from]}]
                             (= pos from))))
        mid (tree state pos)]
    (case dir 1 (or (u/guard R (every-pred some? #(or (same-edge? %) (not (right-edge? %)))))
                    L
                    R
                    mid)
          -1 (or (u/guard L (every-pred some? #(or (same-edge? %) (not (left-edge? %)))))
                 R
                 L
                 mid))))

(defn node-at-cursor
  ([state] (node-at-cursor state (.-from (main-selection state))))
  ([^js state from]
   (let [G__57477 (n/nearest-touching state from -1)
         G__57477 (if (nil? G__57477) nil
       ((fn* [p1__57476#]
         (when
          (or (n/terminal-type? (n/type p1__57476#)) 
              (<= (n/start p1__57476#) from) 
              (<= (n/end p1__57476#) from))
           (let [G__57478 p1__57476#]
            (if (or (n/top? p1__57476#)
                    (and (not (n/terminal-type? (n/type p1__57476#))) 
                         (< (n/start p1__57476#) from (n/end p1__57476#))))
              (first (n/children G__57478 from -1))
              G__57478))))
        G__57477))
     G__57477
     (if (nil? G__57477) nil (uppermost-edge-here from G__57477))]
    (if (nil? G__57477) nil (n/balanced-range state G__57477)))))

(defn cursor-node-string [state]
  (guard (let [selection (node-at-cursor state)]
          (if (nil? selection) nil (range-str state selection)))
           (complement str/blank?)))

(defn eval-at-cursor [viewer]
  (let [cursor-pos (some-> @!viewer .-state .-selection .-main .-head)
        code (first (str/split (str (some-> @!viewer .-state .-doc str)) #" => "))]
    (let [region (cursor-node-string (.-state viewer))
          region (if (nil? region) nil (eval-string region))]
     (if (nil? region) nil (reset! last-result region)))
    (update-editor! (str (subs code 0 cursor-pos)
                         (when-not (= "" (:result @last-result)) " => ")
                         (:result @last-result)
                         (reset! eval-tail (subs code cursor-pos (count code))))
                    cursor-pos)
    (.dispatch @!viewer 
               #js{:selection #js{:anchor cursor-pos
                                  :head   cursor-pos}})))

(defn eval-top-level [viewer]
  (let [region (eval-region/top-level-string (.-state viewer))
        region (if (nil? region) nil (eval-string region))]
    (if (nil? region) nil (reset! last-result region)))
  true)

(defn eval-cell [viewer]
  (reset! last-result (eval-string (str "(do " (.-doc (.-state viewer)) " )")))
  true)

(defn clear-eval []
  (when (not= "" @last-result)
    (reset! last-result "")
    (let [code (-> @!viewer
                   (some-> .-state .-doc str)
                   str
                   (str/split #" => ")
                   first
                   (str @eval-tail))
          cursor-pos (some-> @!viewer .-state .-selection .-main .-head)]
      (update-editor! code (min cursor-pos (count code))))))

(defn extension []
  (.of js/cv.keymap
       (clj->js  [{:key (str "Alt-Enter")
                   :run (partial eval-cell (fn [result] (reset! last-result result)))}
                  {:key  "Mod-Enter"
                   :run (partial eval-top-level (fn [result] (reset! last-result result)))}
                  {:key "Shift-Enter"
                   :run #(eval-at-cursor %)}
                  {:key "Escape" :run clear-eval}
                  {:key "ArrowLeft" :run clear-eval}
                  {:key "ArrowRight" :run clear-eval}])))

(def cm
  (let [doc (str/trim "
(map inc (range 8))
")]
    (js/cm.EditorView. #js {:doc doc
                            :extensions #js [js/cm.basicSetup, (js/lc.clojure), (.highest js/cs.Prec extension)]
                            :parent (js/document.querySelector "#app")
                            #_#_:dispatch (fn [tr] (-> cm (.update #js [tr])) (eval-me))
                            })))
(set! (.-eval_me js/globalThis) eval-me)
(set! (.-cm_instance js/globalThis) cm)

(defn linux? []
  (some? (re-find #"(Linux)|(X11)" js/navigator.userAgent)))

(defn mac? []
  (and (not (linux?))
       (some? (re-find #"(Mac)|(iPhone)|(iPad)|(iPod)" js/navigator.platform))))

(let [elt (js/document.getElementById "evalMe")
      txt (.-innerText elt)
      mod-symbol (if (mac?) "⌘" "⌃")
      txt (str txt " " mod-symbol"-⏎")]
  (set! (.-innerHTML elt) txt))

(eval-me)
