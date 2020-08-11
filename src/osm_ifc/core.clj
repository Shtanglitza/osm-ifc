(ns osm-ifc.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [org.osgeo.proj4j CoordinateTransformFactory
                             CRSFactory
                             ProjCoordinate]))

(defn rand-str [len]
  (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))

(defn create-overpass-ql-query
  ""
  [timeout area types]
  (str "interpreter?data=[out:json]\n"
       "[timeout:" timeout "]\n"
       (str/replace (str "[bbox:" (first area) "," (nth area 1) "," (nth area 2) "," (last area) "];\n"
                         (pr-str (map #(str % ";") types))
                         ";\n out meta; >; out meta qt;") "\"" "")))

(defn get-data
  "I don't do a whole lot."
  [server overpass-ql-query]
  (:body (client/get (str server overpass-ql-query))))

(defn parse-data
  ""
  [s]
  (json/read-json s))

(defn get-coordinates
  ""
  [entity entities]
  (if (= (:type entity) "relation")
    (map #(get-coordinates ((keyword (str (:ref %))) entities) entities) (:members entity))
    (map #(if (contains? entities (keyword (str %)))
            [(:lon ((keyword (str %)) entities))
             (:lat ((keyword (str %)) entities))])
         (:nodes entity))))

(defn transform-coordinates
  ""
  [xs src-crs-s dest-crs-s]                                 ; xs is a collection of vectors with coordinates x and y, src-crs-s and dest-crs-s should in format "EPSG:1234"
  (map #(let [factory (CRSFactory.)
              src-crs (.createFromName factory src-crs-s)
              dest-crs (.createFromName factory dest-crs-s)
              transform (.createTransform (CoordinateTransformFactory.) src-crs dest-crs)
              src-coord (ProjCoordinate. (first %) (last %))
              dest-coord (ProjCoordinate.)]
          [(.x (.transform transform src-coord dest-coord)) (.y (.transform transform src-coord dest-coord))]) xs))

(defn create-owner-history
  ""
  []
  {:IfcOwnerHistory {:OwningUser {:IfcPersonAndOrganization {:ThePerson {:IfcPerson {:FamilyName "Mladenovic",
                                                                                     :GivenName "Vladimir"}},
                                                             :TheOrganization {:IfcOrganization {:Name "Shtanglica",
                                                                                                 :Description "Shtanglica d.o.o."}}}},
                     :OwningApplication {:IfcApplication {:ApplicationDeveloper {:IfcOrganization {:Name "Shtanglica",
                                                                                                   :Description "Shtanglica d.o.o."}},
                                                          :Version "0.1",
                                                          :ApplicationFullName "Overpass IFCer",
                                                          :ApplicationIdentifier "OI 01"}},
                     :ChangeAction :NOTDEFINED,
                     :CreationDate 1592928000}})

(defn create-project
  ""
  [owner-history]
  {:IfcProject {:RepresentationContexts [{:IfcGeometricRepresentationContext {:ContextType "Model",
                                                                              :CoordinateSpaceDimension 3,
                                                                              :Precision 0.01,
                                                                              :WorldCoordinateSystem {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}}],
                :GlobalId (rand-str 22),
                :Name "OSM IFC Project",
                :Description "OSM Data converted to IFC",
                :UnitsInContext {:IfcUnitAssignment {:Units [{:IfcSIUnit {:Dimensions "",
                                                                          :UnitType :LENGTHUNIT,
                                                                          :Name :METRE}}
                                                             {:IfcSIUnit {:Dimensions "",
                                                                          :UnitType :AREAUNIT,
                                                                          :Name :SQUARE_METRE}}
                                                             {:IfcSIUnit {:Dimensions "",
                                                                          :UnitType :VOLUMEUNIT,
                                                                          :Name :CUBIC_METRE}}
                                                             {:IfcSIUnit {:Dimensions "",
                                                                          :UnitType :PLANEANGLEUNIT,
                                                                          :Name :RADIAN}}
                                                             {:IfcSIUnit {:Dimensions "",
                                                                          :UnitType :TIMEUNIT,
                                                                          :Name :SECOND}}]}},
                :OwnerHistory owner-history}})

(defn create-site
  ""
  [owner-history]
  {:IfcSite {:CompositionType :ELEMENT,
             :ObjectPlacement {:IfcLocalPlacement {:RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
             :RefElevation 10.0,
             :RefLatitude '(0 0 0),
             :RefLongitude '(0 0 0),
             :GlobalId (rand-str 22),
             :Name "OSM IFC Site",
             :Description "OSM IFC Site",
             :OwnerHistory owner-history}})

(defn create-building
  ""
  [owner-history building]
  {:IfcBuilding {:CompositionType :ELEMENT,
                 :ObjectPlacement {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                       :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                 :GlobalId (rand-str 22),
                 :BuildingAddress {:IfcPostalAddress {:Town "Leskovac - fix",
                                                      :AddressLines (or (:addr:street (:tags building)) "No adress"),
                                                      :PostalBox (:addr:housenumber (:tags building)),
                                                      :Region "Jablanicki - fix",
                                                      :Country "Serbia - fix",
                                                      :PostalCode (:addr:postcode (:tags building))}},
                 :Name (or (:name (:tags building)) (:addr:housename (:tags building)) "Building"),
                 :Description (or (:name (:tags building)) (:addr:housename (:tags building)) "Building"),
                 :OwnerHistory owner-history}})

(defn create-building-storey
  ""
  [owner-history]
  {:IfcBuildingStorey {:CompositionType :ELEMENT,
                       :ObjectPlacement {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                                                                  :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                             :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                       :GlobalId (rand-str 22),
                       :Name "Storey of a building",
                       :Description "Storey of a building",
                       :OwnerHistory owner-history,
                       :Elevation 0.0}})

(defn get-building-height
  ""
  [building]
  (or (:height building) (when (:building:levels building) (* 3 (:building:levels building))) 10))

(defn create-wall-faces
  ""
  [building coords]
  {:IfcPolygonalFaceSet {:Coordinates {:IfcCartesianPointList3D {:CoordList (map #(apply list %) (reduce (fn [x y]
                                                                                                         (conj x
                                                                                                               (conj y 0)
                                                                                                               (conj y (get-building-height building))))
                                                                                                       []
                                                                                                       coords))}},
                         :Closed :TRUE,
                         :Faces (into [] (concat (map #(into {} {:IfcIndexedPolygonalFace {:CoordIndex (list % (+ % 1) (+ % 3) (+ % 2))}})
                                                      (filter odd? (range (* 2 (- (count coords) 1)))))
                                                 [{:IfcIndexedPolygonalFace {:CoordIndex (list (- (count coords) 1) (count coords) 2 1)}}]))}})

(defn create-wall
  ""
  [owner-history building coords dest-crs-s]
  {:IfcWall {:ObjectPlacement {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                                                                                             :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                                                        :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                   :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
             :GlobalId (rand-str 22),
             :Representation {:IfcProductDefinitionShape {:Representations [{:IfcShapeRepresentation {:ContextOfItems {:IfcGeometricRepresentationSubContext {:TargetView :MODEL_VIEW,
                                                                                                                                                              :ParentContext {:IfcGeometricRepresentationContext {:ContextType "Model",
                                                                                                                                                                                                                  :CoordinateSpaceDimension 3,
                                                                                                                                                                                                                  :Precision 0.01,
                                                                                                                                                                                                                  :WorldCoordinateSystem {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                                                                                                                              :WorldCoordinateSystem "",
                                                                                                                                                              :CoordinateSpaceDimension "",
                                                                                                                                                              :ContextIdentifier "Body",
                                                                                                                                                              :ContextType "Model",
                                                                                                                                                              :TargetScale 0.0,
                                                                                                                                                              :Precision "",
                                                                                                                                                              :TrueNorth ""}},
                                                                                                      :RepresentationIdentifier "Body",
                                                                                                      :RepresentationType "Tessellation",
                                                                                                      :Items [(create-wall-faces building (transform-coordinates coords "EPSG:4326" dest-crs-s))]}}]}},
             :Name (str "Wall of " (or (:name (:tags building)) (:addr:housename (:tags building)) "building")),
             :Description "Wall of a building",
             :OwnerHistory owner-history}})

(defn create-inner-coord-indices
  [xs n]
  (let [coll (atom (range n (+ n (apply + xs))))]
    (reduce
      (fn [out chunk-size]
        (let [[chunk resto] (split-at chunk-size @coll)
              _ (reset! coll resto)]
          (conj out chunk)))
      [] xs)))

(defn create-roof-faces
  ""
  [building coords]
  (if (= (:type building) "relation")
    (let [coords-per-way (map #(count %) coords)]
      {:IfcPolygonalFaceSet {:Coordinates {:IfcCartesianPointList3D {:CoordList (partition 3 (flatten coords))}},
                           :Closed :TRUE,
                           :Faces [{:IfcIndexedPolygonalFaceWithVoids {:CoordIndex (map inc (range (count (first coords)))),
                                                                       :InnerCoordIndices (create-inner-coord-indices coords-per-way (count (first coords)))}}]}})
    {:IfcPolygonalFaceSet {:Coordinates {:IfcCartesianPointList3D {:CoordList (map #(into '() %) (map #(into '() (conj % (get-building-height building))) coords))}},
                           :Closed :TRUE,
                           :Faces {:IfcIndexedPolygonalFace {:CoordIndex (map inc (range (count coords)))}}}}))

(defn create-roof
  ""
  [owner-history building coords dest-crs-s]
  {:IfcRoof {:ObjectPlacement {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:PlacementRelTo {:IfcLocalPlacement {:RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                                                                                             :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                                                        :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                   :RelativePlacement {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
             :GlobalId (rand-str 22),
             :Representation {:IfcProductDefinitionShape {:Representations [{:IfcShapeRepresentation {:ContextOfItems {:IfcGeometricRepresentationSubContext {:TargetView :MODEL_VIEW,
                                                                                                                                                              :ParentContext {:IfcGeometricRepresentationContext {:ContextType "Model",
                                                                                                                                                                                                                  :CoordinateSpaceDimension 3,
                                                                                                                                                                                                                  :Precision 0.01,
                                                                                                                                                                                                                  :WorldCoordinateSystem {:IfcAxis2Placement3D {:Location {:IfcCartesianPoint {:Coordinates '(0.0 0.0 0.0)}}}}}},
                                                                                                                                                              :WorldCoordinateSystem "",
                                                                                                                                                              :CoordinateSpaceDimension "",
                                                                                                                                                              :ContextIdentifier "Body",
                                                                                                                                                              :ContextType "Model",
                                                                                                                                                              :TargetScale 0.0,
                                                                                                                                                              :Precision "",
                                                                                                                                                              :TrueNorth ""}},
                                                                                                      :RepresentationIdentifier "Body",
                                                                                                      :RepresentationType "Tessellation",
                                                                                                      :Items [(create-roof-faces building (transform-coordinates coords "EPSG:4326" dest-crs-s))]}}]}},
             :Name (str "Roof of " (or (:name (:tags building)) (:addr:housename (:tags building)) "Building")),
             :Description "Roof of a building",
             :OwnerHistory owner-history}})

(defn create-IfcRelAggregates-container
  ""
  [parent-el child-els owner-history]                                     ; child-els is a vector
  {:IfcRelAggregates {:GlobalId (rand-str 22),
                      :OwnerHistory owner-history,
                      :Name (str (name (first (keys parent-el))) " container"),
                      :Description (str (name (first (keys parent-el))) " container for " (str (name (first (keys (first child-els))))) "s"),
                      :RelatingObject parent-el,
                      :RelatedObjects child-els}})

(defn create-IfcRelContainedInSpatialStructure-container
  ""
  [parent-el child-els owner-history]                                     ; child-els is a vector
  {:IfcRelContainedInSpatialStructure {:GlobalId (rand-str 22),
                                       :OwnerHistory owner-history,
                                       :Name (str "Spatial elements of " (or (:name (:tags parent-el)) (:addr:housename (:tags parent-el)) " storey")),
                                       :Description "Contents of building storey",
                                       :RelatedElements child-els,
                                       :RelatingStructure parent-el}})

(defn create-clj-struct2
  ""
  [entities]
  (let [random-building (first (filter #(and (= (:type %) "way") (:building (:tags %))) (:elements entities)))
        owner-history (create-owner-history)
        project (create-project owner-history)
        site (create-site owner-history)
        buildings [(create-building owner-history random-building)]
        storeys (repeatedly (count buildings) #(create-building-storey owner-history))]
    (list (create-IfcRelAggregates-container project [site] owner-history)
          (create-IfcRelAggregates-container site buildings owner-history)
          (map #(create-IfcRelAggregates-container % [(create-building-storey owner-history)] owner-history) buildings)
          (map #(create-IfcRelContainedInSpatialStructure-container % [(create-wall owner-history (first buildings) (get-coordinates random-building entities) "EPSG:3909")
                                                                       (create-roof owner-history (first buildings) (get-coordinates random-building entities) "EPSG:3909")] owner-history) storeys))))

(defn create-clj-struct
  ""
  [entities]
  (let [owner-history (create-owner-history)
        project (create-project owner-history)
        site (create-site owner-history)
        ;; FIXME: add check for relations and disable ways that are into existing relations
        buildings-data (filter #(and (:building (:tags %)) (= (:type %) "way")) (vals entities))
        buildings (map #(create-building owner-history %) buildings-data)
        storeys (repeatedly (count buildings) #(create-building-storey owner-history))]
    (concat (create-IfcRelAggregates-container project [site] owner-history)
            (create-IfcRelAggregates-container site buildings owner-history)
            (map #(create-IfcRelAggregates-container %1 [%2] owner-history) buildings storeys)
            (map #(create-IfcRelContainedInSpatialStructure-container %2 [(create-wall owner-history %1 (get-coordinates %1 entities) "EPSG:3909")
                                                                          (create-roof owner-history %1 (get-coordinates %1 entities) "EPSG:3909")] owner-history) buildings-data storeys))))


(comment

  (def x (get-data "https://www.overpass-api.de/api/"
                   (create-overpass-ql-query 60
                                             ; WGS 84 lat-lon
                                             '(42.994020 ;south
                                                21.945030 ;west
                                                42.997748 ;north
                                                21.958337) ;east
                                             '("rel[building]"
                                                "way[building]"
                                                "node[building]"
                                                "rel[highway]"
                                                "way[highway]"
                                                "node[highway]"))))

  (def y (parse-data x))

  (def z (into {} (map #(hash-map (keyword (str (:id %))) %) (:elements y))))

  (def o (filter #(and (:building (:tags %)) (= (:type %) "way")) (vals z)))

  (def p (map #(get-coordinates % z) o))

  (map #(if (contains? z (keyword (str %))) ((keyword (str %)) z))
       (:nodes (first (filter #(and (= (:type %) "way") (:building (:tags %))) (:elements y)))))

  (def k (map #(if (contains? z (keyword (str %))) {(keyword (str %)) [(:lon ((keyword (str %)) z)) (:lat ((keyword (str %)) z))]})
              (:nodes (first (filter #(and (= (:type %) "way") (:building (:tags %))) (:elements y))))))

  (def l (get-coordinates (first (filter #(and (= (:type %) "way") (:building (:tags %))) (:elements y)))
                          z))

  (def buildings (filter #(and (:building (:tags %)) (= (:type %) "way")) (:elements y)))

  (reduce (fn [x y]
            (conj x
                  (conj y 0)
                  (conj y 10)))
          []
          l)

  ;(pprint/pprint y)

  ;(pprint/pprint z)

  ;(pprint/pprint k)

  (pprint/pprint l)

  (filter #(:building (:tags (val %))) z)
  (filter #(:highway (:tags (val %))) z)

  )