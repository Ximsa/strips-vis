(ns strips-vis.core
  (:gen-class)
  (:require
   [clojure.set :as set])
  (:use [tangle.core]))

(defn add-new-node [edge-label original del add {edges :edges nodes :nodes}]
  (let [new-node (apply conj (apply disj original del) add)]
    {:edges (conj edges [original new-node edge-label])
     :nodes (conj nodes new-node)}))
        
    
  

(defn apply-rule [rule-name
                  {pre :pre del :del add :add}
                  graph]
  (let [selected-nodes (filter (fn [node] (set/subset? pre node))
                               (:nodes graph))
        updated-graph (reduce (fn [graph node]
                                (add-new-node rule-name
                                              node
                                              del
                                              add
                                              graph))
                              graph
                              selected-nodes)]
    updated-graph))

(defn update-graph [rules graph]
  (reduce-kv (fn [graph rule-name rule]
               (apply-rule rule-name rule graph))
          graph
          rules))

(defn translate-strips [rules graph]
  (reduce (fn [old new] (if (= old new)
                          (reduced new)
                          new))
          {}
          (iterate (partial update-graph rules) graph)))

(defn state-to-string [state]
  (reduce (fn [result elem] (str result " " elem))
          ""
          (sort (vec state))))

(defn convert-edge [[from to label]]
  [(state-to-string from)
   (state-to-string to)
   {:label label}])

(defn graph-to-dot [{edges :edges nodes :nodes}]
  (let [converted-nodes (map state-to-string nodes)
        converted-edges (map convert-edge edges)]
    (graph->dot converted-nodes converted-edges
                {:node {:shape :box}
                 :directed? true})))

(defn state-in-graph? [{nodes :nodes} state]
  (->> nodes
       (filter (partial = state))
       empty?
       not))

(defn get-state-transitions [{edges :edges} state]
  (filter (fn [[from to transition]] (or (= state from)
                                         (= state to)))
          edges))

(def initial-state #{"BoatLeft" "BoatEmpty" "WolfLeft" "SheepLeft" "CabbageLeft"})

(def initial-graph {:edges #{}
                    :nodes #{initial-state}})

(def rules {"loadWolfLeft"
            {:pre #{"BoatLeft" "BoatEmpty" "WolfLeft"}
             :del #{"BoatEmpty" "WolfLeft"}
             :add #{"WolfInBoat"}}
            "loadCabbageLeft"
            {:pre #{"BoatLeft" "BoatEmpty" "CabbageLeft"}
             :del #{"BoatEmpty" "CabbageLeft"}
             :add #{"CabbageInBoat"}}
            "loadSheepLeft"
            {:pre #{"BoatLeft" "BoatEmpty" "SheepLeft"}
             :del #{"BoatEmpty" "SheepLeft"}
             :add #{"SheepInBoat"}}
            
            "loadWolfRight"
            {:pre #{"BoatRight" "BoatEmpty" "WolfRight"}
             :del #{"BoatEmpty" "WolfRight"}
             :add #{"WolfInBoat"}}
            "loadCabbageRight"
            {:pre #{"BoatRight" "BoatEmpty" "CabbageRight"}
             :del #{"BoatEmpty" "CabbageRight"}
             :add #{"CabbageInBoat"}}
            "loadSheepRight"
            {:pre #{"BoatRight" "BoatEmpty" "SheepRight"}
             :del #{"BoatEmpty" "SheepRight"}
             :add #{"SheepInBoat"}}

            "unloadWolfLeft"
            {:pre #{"BoatLeft" "WolfInBoat"}
             :del #{"WolfInBoat"}
             :add #{"BoatEmpty" "WolfLeft"}}
            "unloadSheepLeft"
            {:pre #{"BoatLeft" "SheepInBoat"}
             :del #{"SheepInBoat"}
             :add #{"BoatEmpty" "SheepLeft"}}
            "unloadCabbageLeft"
            {:pre #{"BoatLeft" "CabbageInBoat"}
             :del #{"CabbageInBoat"}
             :add #{"BoatEmpty" "CabbageLeft"}}

            "unloadWolfRight"
            {:pre #{"BoatRight" "WolfInBoat"}
             :del #{"WolfInBoat"}
             :add #{"BoatEmpty" "WolfRight"}}
            "unloadSheepRight"
            {:pre #{"BoatRight" "SheepInBoat"}
             :del #{"SheepInBoat"}
             :add #{"BoatEmpty" "SheepRight"}}
            "unloadCabbageRight"
            {:pre #{"BoatRight" "CabbageInBoat"}
             :del #{"CabbageInBoat"}
             :add #{"BoatEmpty" "CabbageRight"}}

            "moveToLeft1"
            {:pre #{"BoatRight" "WolfRight" "SheepRight"}
             :del #{"BoatRight" "SheepRight"}
             :add #{"BoatLeft"}}
            "moveToLeft2"
            {:pre #{"BoatRight" "SheepLeft"}
             :del #{"BoatRight"}
             :add #{"BoatLeft"}}
            "moveToLeft3"
            {:pre #{"BoatRight" "SheepInBoat"}
             :del #{"BoatRight"}
             :add #{"BoatLeft"}}
            "moveToLeft4"
            {:pre #{"BoatRight" "SheepRight" "CabbageRight"}
             :del #{"BoatRight" "CabbageRight"}
             :add #{"BoatLeft"}}
            "moveToLeft5"
            {:pre #{"BoatRight" "WolfInBoat" "CabbageLeft"}
             :del #{"BoatRight"}
             :add #{"BoatLeft"}}
            "moveToLeft6"
            {:pre #{"BoatRight" "WolfLeft" "CabbageLeft"}
             :del #{"BoatRight"}
             :add #{"BoatLeft"}}
            "moveToLeft7"
            {:pre #{"BoatRight" "WolfLeft" "CabbageInBoat"}
             :del #{"BoatRight"}
             :add #{"BoatLeft"}}

            "moveToRight1"
            {:pre #{"BoatLeft" "WolfLeft" "SheepLeft"}
             :del #{"BoatLeft" "SheepLeft"}
             :add #{"BoatRight"}}
            "moveToRight2"
            {:pre #{"BoatLeft" "SheepRight"}
             :del #{"BoatLeft"}
             :add #{"BoatRight"}}
            "moveToRight3"
            {:pre #{"BoatLeft" "SheepInBoat"}
             :del #{"BoatLeft"}
             :add #{"BoatRight"}}
            "moveToRight4"
            {:pre #{"BoatLeft" "SheepLeft" "CabbageLeft"}
             :del #{"BoatLeft" "CabbageLeft"}
             :add #{"BoatRight"}}
            "moveToRight5"
            {:pre #{"BoatLeft" "WolfInBoat" "CabbageRight"}
             :del #{"BoatLeft"}
             :add #{"BoatRight"}}
            "moveToRight6"
            {:pre #{"BoatLeft" "WolfRight" "CabbageRight"}
             :del #{"BoatLeft"}
             :add #{"BoatRight"}}
            "moveToRight7"
            {:pre #{"BoatLeft" "WolfRight" "CabbageInBoat"}
             :del #{"BoatLeft"}
             :add #{"BoatRight"}}})

(def graph (translate-strips rules initial-graph))

(->> (translate-strips rules initial-graph)
    graph-to-dot
    (spit "graph.dot"))
