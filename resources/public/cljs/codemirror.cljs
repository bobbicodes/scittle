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

(defn eval-at-cursor [viewer]
  (let [cursor-pos (some-> @!viewer .-state .-selection .-main .-head)
        code (first (str/split (str (some-> @!viewer .-state .-doc str)) #" => "))]
    (let [region (str "(do " (.-doc (.-state viewer)) " )")
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
  (let [region (str "(do " (.-doc (.-state viewer)) " )")
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
