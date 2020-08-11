(ns osm-ifc.flat
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [ifc-tools-clj.step.core :as ifc-step])
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
       (str/replace (str "[bbox:" (first area) "," (nth area 1) "," (nth area 2) "," (nth area 3) "];\n"
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

(defn create-map-from-parsed-data
  ""
  [m]
  (into {} (map #(hash-map (keyword (str (:id %))) %) (:elements m))))

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

(defn get-building-height
  ""
  [building]
  (or (:height building) (when (:building:levels building) (* 3 (:building:levels building))) 10))

(defn add-third-dimension
  ""
  [coords building-data]
  (map #(apply list %) (reduce (fn [x y]
                                 (conj x
                                       (conj y 0)
                                       (conj y (get-building-height building-data))))
                               []
                               coords)))

(defn create-inner-coord-indexes
  ""
  [xs n]
  (let [coll (atom (range n (+ n (apply + xs))))]
    (reduce
      (fn [out chunk-size]
        (let [[chunk resto] (split-at chunk-size @coll)
              _ (reset! coll resto)]
          (conj out chunk)))
      [] xs)))

(defn create-flat-entity
  ""
  [id type params]
  {:id id
   :type-ref type
   :params params})

(defn create-flat-owner-history
  ""
  [id]
  (list (create-flat-entity (str "#" id)
                            :IfcOwnerHistory
                            {:OwningUser (str "#" (+ id 1))
                             :OwningApplication (str "#" (+ id 4))
                             :State "$"
                             :ChangeAction :NOTDEFINED
                             :LastModifiedDate "$"
                             :LastModifyingUser "$"
                             :LastModifyingApplication "$"
                             :CreationDate (quot (System/currentTimeMillis) 1000)})
        (create-flat-entity (str "#" (+ id 1))
                            :IfcPersonAndOrganization
                            {:ThePerson (str "#" (+ id 2))
                             :TheOrganization (str "#" (+ id 3))
                             :Roles "$"})
        (create-flat-entity (str "#" (+ id 2))
                            :IfcPerson
                            {:Identification "$"
                             :FamilyName "John"
                             :GivenName "Smith"
                             :MiddleNames "$"
                             :PrefixTitles "$"
                             :SuffixTitles "$"
                             :Roles "$"
                             :Addresses "$"})
        (create-flat-entity (str "#" (+ id 3))
                            :IfcOrganization
                            {:Identification "$"
                             :Name "Org"
                             :Description "Org Ltd."
                             :Roles "$"
                             :Addresses "$"})
        (create-flat-entity (str "#" (+ id 4))
                            :IfcApplication
                            {:ApplicationDeveloper (str "#" (+ id 3))
                             :Version "0.1"
                             :ApplicationFullName "OSM ifc"
                             :ApplicationIdentifier "OSM ifc 1"})))

(defn create-flat-project
  ""
  [id rc-id uic-id oh-id]
  (create-flat-entity (str "#" id)
                      :IfcProject
                      {:RepresentationContexts (list (str "#" rc-id))
                       :ObjectType "$"
                       :GlobalId (rand-str 22)
                       :Name "OSM IFC Project"
                       :Description "OSM Data converted to IFC"
                       :UnitsInContext (str "#" uic-id)
                       :OwnerHistory (str "#" oh-id)
                       :LongName "$"
                       :Phase "$"}))

(defn create-flat-representation-contexts
  ""
  [id]
  (list (create-flat-entity (str "#" id)
                            :IfcGeometricRepresentationContext
                            {:ContextIdentifier "$",
                             :ContextType "Model",
                             :CoordinateSpaceDimension 3,
                             :Precision 0.01,
                             :WorldCoordinateSystem (str "#" (+ id 1)),
                             :TrueNorth "$"})
        (create-flat-entity (str "#" (+ id 1))
                            :IfcAxis2Placement3D
                            {:Location (str "#" (+ id 2))
                             :Axis "$"
                             :RefDirection "$"})
        (create-flat-entity (str "#" (+ id 2))
                            :IfcCartesianPoint
                            {:Coordinates (list 0.0 0.0 0.0)})
        (create-flat-entity (str "#" (+ id 3))
                            :IfcGeometricRepresentationSubContext
                            {:TargetView :MODEL_VIEW
                             :ParentContext (str "#" id)
                             :WorldCoordinateSystem ""
                             :CoordinateSpaceDimension ""
                             :ContextIdentifier "Axis"
                             :ContextType "Model"
                             :UserDefinedTargetView "$"
                             :TargetScale 0.0
                             :Precision ""
                             :TrueNorth ""})
        (create-flat-entity (str "#" (+ id 4))
                            :IfcGeometricRepresentationSubContext
                            {:TargetView :MODEL_VIEW
                             :ParentContext (str "#" id)
                             :WorldCoordinateSystem ""
                             :CoordinateSpaceDimension ""
                             :ContextIdentifier "Body"
                             :ContextType "Model"
                             :UserDefinedTargetView "$"
                             :TargetScale 0.0
                             :Precision ""
                             :TrueNorth ""})))

(defn create-flat-units-in-context
  ""
  [id]
  (list (create-flat-entity (str "#" id)
                            :IfcUnitAssignment
                            {:Units (map #(str "#" %) (range (+ id 1) (+ id 6)))})
        (create-flat-entity (str "#" (+ id 1))
                            :IfcSIUnit
                            {:Dimensions "", :UnitType :PLANEANGLEUNIT, :Prefix "$", :Name :RADIAN})
        (create-flat-entity (str "#" (+ id 2))
                            :IfcSIUnit
                            {:Dimensions "", :UnitType :TIMEUNIT, :Prefix "$", :Name :SECOND})
        (create-flat-entity (str "#" (+ id 3))
                            :IfcSIUnit
                            {:Dimensions "", :UnitType :LENGTHUNIT, :Prefix "$", :Name :METRE})
        (create-flat-entity (str "#" (+ id 4))
                            :IfcSIUnit
                            {:Dimensions "", :UnitType :AREAUNIT, :Prefix "$", :Name :SQUARE_METRE})
        (create-flat-entity (str "#" (+ id 5))
                            :IfcSIUnit
                            {:Dimensions "", :UnitType :VOLUMEUNIT, :Prefix "$", :Name :CUBIC_METRE})))

(defn create-flat-site
  ""
  [id oh-id rc-id p-id]
  (list (create-flat-entity (str "#" id)
                            :IfcSite
                            {:CompositionType :ELEMENT
                             :ObjectPlacement (str "#" (+ id 1))
                             :RefElevation 0.0
                             :SiteAddress "$"
                             :RefLatitude '(0 0 0)
                             :LandTitleNumber "$"
                             :RefLongitude '(0 0 0)
                             :ObjectType "$"
                             :GlobalId (rand-str 22)
                             :Representation "$"
                             :Name "OSM IFC Site"
                             :Description "OSM IFC Site"
                             :OwnerHistory (str "#" oh-id)
                             :LongName "$"})
        (create-flat-entity (str "#" (+ id 1))
                            :IfcLocalPlacement
                            {:PlacementRelTo "$"
                             :RelativePlacement (str "#" (+ id 2))})
        (create-flat-entity (str "#" (+ id 2))
                            :IfcAxis2Placement3D
                            {:Location (str "#" (+ rc-id 2))
                             :Axis "$"
                             :RefDirection "$"})
        (create-flat-entity (str "#" (+ id 3))
                            :IfcRelAggregates
                            {:GlobalId (rand-str 22)
                             :OwnerHistory (str "#" oh-id)
                             :Name "Project container"
                             :Description "Project container for sites."
                             :RelatingObject (str "#" p-id)
                             :RelatedObjects (list (str "#" id))})))

(defn create-flat-building
  ""
  [id s-id rc-id oh-id building-data]
  (list (create-flat-entity (str "#" id)
                            :IfcBuilding
                            {:CompositionType :ELEMENT
                             :ObjectPlacement (str "#" (+ id 1))
                             :ElevationOfRefHeight "$"
                             :ObjectType "$"
                             :GlobalId (rand-str 22)
                             :Representation "$"
                             :BuildingAddress (str "#" (+ id 3))
                             :Name (or (:name (:tags building-data)) "Building")
                             :Description (or (:description (:tags building-data))
                                              (when-not (= (:building (:tags building-data)) "yes")
                                                (str (:building (:tags building-data)) " building"))
                                              "Building")
                             :OwnerHistory (str "#" oh-id)
                             :ElevationOfTerrain "$"
                             :LongName "$"})
        (create-flat-entity (str "#" (+ id 1))
                            :IfcLocalPlacement
                            {:PlacementRelTo (str "#" (+ s-id 1))
                             :RelativePlacement (str "#" (+ id 2))})
        (create-flat-entity (str "#" (+ id 2))
                            :IfcAxis2Placement3D
                            {:Location (str "#" (+ rc-id 2))
                             :Axis "$"
                             :RefDirection "$"})
        (create-flat-entity (str "#" (+ id 3))
                            :IfcPostalAddress
                            {:Town (or (:addr:city (:tags building-data)) "Spatial intersection to be fixed in future releases")
                             :AddressLines (list (or (:addr:street (:tags building-data)) "No address"))
                             :Purpose "$"
                             :PostalBox (or (:addr:housenumber (:tags building-data)) "No postal box")
                             :UserDefinedPurpose "$"
                             :InternalLocation "$"
                             :Region (or (:addr:region (:tags building-data)) "Spatial intersection to be fixed in future releases")
                             :Description "$"
                             :Country (or (:addr:country (:tags building-data)) "Spatial intersection to be fixed in future releases")
                             :PostalCode (or (:addr:postcode (:tags building-data)) "")})
        (create-flat-entity (str "#" (+ id 4))
                            :IfcBuildingStorey
                            {:CompositionType :ELEMENT
                             :ObjectPlacement (str "#" (+ id 5))
                             :ObjectType "$"
                             :GlobalId (rand-str 22)
                             :Representation "$"
                             :Name (str (or (:name (:tags building-data)) "building") " storey")
                             :Description "building storey"
                             :OwnerHistory (str "#" oh-id)
                             :Elevation 0.0
                             :LongName "$"})
        (create-flat-entity (str "#" (+ id 5))
                            :IfcLocalPlacement
                            {:PlacementRelTo (str "#" (+ id 1))
                             :RelativePlacement (str "#" (+ id 6))})
        (create-flat-entity (str "#" (+ id 6))
                            :IfcAxis2Placement3D
                            {:Location (str "#" (+ rc-id 2))
                             :Axis "$"
                             :RefDirection "$"})
        (create-flat-entity (str "#" (+ id 7))
                            :IfcRelAggregates
                            {:GlobalId (rand-str 22)
                             :OwnerHistory (str "#" oh-id)
                             :Name (str (or (:name (:tags building-data)) "Building") " container for storeys")
                             :Description "Building container for storeys"
                             :RelatingObject (str "#" id)
                             :RelatedObjects (str "#" (+ id 4))})))

(defn create-flat-wall
  ""
  [id b-id rc-id oh-id coords dest-crs-s building-data]
  (flatten
    (list
      (create-flat-entity (str "#" id)
                          :IfcWall
                          {:ObjectPlacement (str "#" (+ id 1))
                           :Tag "$"
                           :ObjectType "$"
                           :GlobalId (rand-str 22)
                           :Representation (str "#" (+ id 6))
                           :Name "Wall"
                           :Description "Wall"
                           :OwnerHistory (str "#" oh-id)
                           :PredefinedType "$"})
      (create-flat-entity (str "#" (+ id 1))
                          :IfcLocalPlacement
                          {:PlacementRelTo (str "#" (+ b-id 5))
                           :RelativePlacement (str "#" (+ id 2))})
      (create-flat-entity (str "#" (+ id 2))
                          :IfcAxis2Placement3D
                          {:Location (str "#" (+ rc-id 2))
                           :Axis "$"
                           :RefDirection "$"})
      (create-flat-entity (str "#" (+ id 3))
                          :IfcShapeRepresentation
                          {:ContextOfItems (str "#" (+ rc-id 4))
                           :RepresentationIdentifier "Body"
                           :RepresentationType "Tessellation"
                           :Items (list (str "#" (+ id 5)))})
      (create-flat-entity (str "#" (+ id 4))
                          :IfcCartesianPointList3D
                          {:CoordList (add-third-dimension (transform-coordinates coords "EPSG:4326" dest-crs-s) building-data)})
      (create-flat-entity (str "#" (+ id 5))
                          :IfcPolygonalFaceSet
                          {:Coordinates (str "#" (+ id 4))
                           :Closed :TRUE
                           :Faces (map #(str "#" %) (range (+ id 7) (+ id 7 (count coords))))
                           :PnIndex "$"})
      (create-flat-entity (str "#" (+ id 6))
                          :IfcProductDefinitionShape
                          {:Name "$"
                           :Description "$"
                           :Representations (list (str "#" (+ id 3)))})
      (map #(create-flat-entity (str "#" (- (+ id 7 %) (/ % 2)))
                                :IfcIndexedPolygonalFace
                                {:CoordIndex (list (+ % 1) (+ % 2) (+ % 4) (+ % 3))})
           (range 0 (* (- (count coords) 1) 2) 2))
      (create-flat-entity (str "#" (+ id 6 (count coords)))
                          :IfcIndexedPolygonalFace
                          {:CoordIndex (list (- (* (count coords) 2) 1) (* (count coords) 2) 2 1)}))))

(defn create-flat-roof
  ""
  ([id b-id rc-id oh-id coords dest-crs-s building-data]
   (flatten
     (list
       (create-flat-entity (str "#" id)
                           :IfcRoof
                           {:ObjectPlacement (str "#" (+ id 1))
                            :Tag "$"
                            :ObjectType "$"
                            :GlobalId (rand-str 22)
                            :Representation (str "#" (+ id 6))
                            :Name "Roof for Test Example"
                            :Description "Description of Roof"
                            :OwnerHistory (str "#" oh-id)
                            :PredefinedType "$"})
       (create-flat-entity (str "#" (+ id 1))
                           :IfcLocalPlacement
                           {:PlacementRelTo (str "#" (+ b-id 5))
                            :RelativePlacement (str "#" (+ id 2))})
       (create-flat-entity (str "#" (+ id 2))
                           :IfcAxis2Placement3D
                           {:Location (str "#" (+ rc-id 2))
                            :Axis "$"
                            :RefDirection "$"})
       (create-flat-entity (str "#" (+ id 3))
                           :IfcShapeRepresentation
                           {:ContextOfItems (str "#" (+ rc-id 4))
                            :RepresentationIdentifier "Body"
                            :RepresentationType "Tessellation"
                            :Items (list (str "#" (+ id 5)))})
       (create-flat-entity (str "#" (+ id 4))
                           :IfcCartesianPointList3D
                           {:CoordList (map #(concat % [(get-building-height building-data)])
                                            (transform-coordinates coords "EPSG:4326" dest-crs-s))})
       (create-flat-entity (str "#" (+ id 5))
                           :IfcPolygonalFaceSet
                           {:Coordinates (str "#" (+ id 4))
                            :Closed :TRUE
                            :Faces (list (str "#" (+ id 7)))
                            :PnIndex "$"})
       (create-flat-entity (str "#" (+ id 6))
                           :IfcProductDefinitionShape
                           {:Name "$"
                            :Description "$"
                            :Representations (list (str "#" (+ id 3)))})
       (create-flat-entity (str "#" (+ id 7))
                           :IfcIndexedPolygonalFace
                           {:CoordIndex (range 1 (+ (count coords) 1))}))))
  ([id b-id rc-id oh-id coords dest-crs-s building-data void-indexes]
   (flatten
     (list
       (create-flat-entity (str "#" id)
                           :IfcRoof
                           {:ObjectPlacement (str "#" (+ id 1))
                            :Tag "$"
                            :ObjectType "$"
                            :GlobalId (rand-str 22)
                            :Representation (str "#" (+ id 3))
                            :Name "Roof for Test Example"
                            :Description "Description of Roof"
                            :OwnerHistory (str "#" oh-id)
                            :PredefinedType "$"})
       (create-flat-entity (str "#" (+ id 1))
                           :IfcLocalPlacement
                           {:PlacementRelTo (str "#" (+ b-id 5))
                            :RelativePlacement (str "#" (+ id 2))})
       (create-flat-entity (str "#" (+ id 2))
                           :IfcAxis2Placement3D
                           {:Location (str "#" (+ rc-id 2))
                            :Axis "$"
                            :RefDirection "$"})
       (create-flat-entity (str "#" (+ id 3))
                           :IfcProductDefinitionShape
                           {:Name "$"
                            :Description "$"
                            :Representations (list "$" (str "#" (+ id 7)))})
       (create-flat-entity (str "#" (+ id 4))
                           :IfcCartesianPointList3D
                           {:CoordList (map #(concat % [(get-building-height building-data)])
                                            (transform-coordinates (partition 2 (flatten coords)) "EPSG:4326" dest-crs-s))})
       (create-flat-entity (str "#" (+ id 5))
                           :IfcIndexedPolygonalFaceWithVoids
                           {:CoordIndex (range 1 (+ (first void-indexes) 1))
                            :InnerCoordIndices (reverse (into '() (create-inner-coord-indexes (rest void-indexes)
                                                                                              (+ (first void-indexes) 1))))})
       (create-flat-entity (str "#" (+ id 6))
                           :IfcPolygonalFaceSet
                           {:Coordinates (str "#" (+ id 4))
                            :Closed :TRUE
                            :Faces (list (str "#" (+ id 5)))
                            :PnIndex "$"})
       (create-flat-entity (str "#" (+ id 7))
                           :IfcShapeRepresentation
                           {:ContextOfItems (str "#" (+ rc-id 4))
                            :RepresentationIdentifier "Body"
                            :RepresentationType "Tessellation"
                            :Items (list (str "#" (+ id 6)))})))))

(defn create-flat-rel-aggregates
  ""
  [id oh-id name descpription relating-object-id related-objects-ids]
  (create-flat-entity (str "#" id)
                      :IfcRelAggregates
                      {:GlobalId (rand-str 22)
                       :OwnerHistory (str "#" oh-id)
                       :Name name
                       :Description descpription
                       :RelatingObject relating-object-id
                       :RelatedObjects related-objects-ids}))

(defn create-flat-rel-contained-in-spatial-structure
  ""
  [id oh-id name description related-elements-ids relating-structure-id]
  (create-flat-entity (str "#" id)
                      :IfcRelContainedInSpatialStructure
                      {:GlobalId (rand-str 22)
                       :OwnerHistory (str "#" oh-id)
                       :Name name
                       :Description description
                       :RelatedElements (map #(str "#" %) related-elements-ids),
                       :RelatingStructure (str "#" relating-structure-id)}))

(defn create-flat-map
  ""
  [dest-crs entities]
  (let [buildings (filter #(:building (:tags %)) (vals entities))]
    (flatten
      (list
        (create-flat-project 1 7 12 2)
        (create-flat-owner-history 2)
        (create-flat-representation-contexts 7)
        (create-flat-units-in-context 12)
        (create-flat-site 18 2 7 1)
        (map (fn
               [building id]
               (let [walls-coords-sx (if (seq? (first (get-coordinates building entities)))
                                       (get-coordinates building entities)
                                       (list (get-coordinates building entities)))
                     coords (get-coordinates building entities)]
                 (list
                   (create-flat-building (* id 500) 18 7 2 building)
                   (map #(create-flat-wall (+ (* id 500) (* %2 50)) (* id 500) 7 2 (if (coll? %1) %1 walls-coords-sx) dest-crs building) walls-coords-sx (map inc (range (count walls-coords-sx))))
                   (if (= (:type building) "way")
                     (create-flat-roof (+ (* id 500) 450) (* id 500) 7 2 coords dest-crs building)
                     (create-flat-roof (+ (* id 500) 450) (* id 500) 7 2 coords dest-crs building (map (fn [sx] (count sx)) coords)))))) buildings (map inc (range (count buildings))))))))

(defn save-to-file
  ""
  [file-loc server timeout bbox classes dest-crs]
  (->> (get-data server (create-overpass-ql-query timeout bbox classes))
       (parse-data)
       (create-map-from-parsed-data)
       (create-flat-map dest-crs)
       ifc-step/ifc-serialize*
       (spit (str file-loc "osm" (quot (System/currentTimeMillis) 1000) ".ifc"))))

(defn create-data
  ""
  [server timeout bbox classes dest-crs]
  (->> (get-data server (create-overpass-ql-query timeout bbox classes))
       (parse-data)
       (create-map-from-parsed-data)
       (create-flat-map dest-crs)
       ifc-step/ifc-serialize*))

(comment

  (save-to-file "/home/muta93srb/Documents/" "https://www.overpass-api.de/api/" 60 '(44.804955 20.474496 44.806280 20.477800) '("rel[building]" "way[building]" "node[building]" "rel[highway]" "way[highway]" "node[highway]") "EPSG:3909")
  (create-data "https://www.overpass-api.de/api/" 60 '(44.804955 20.474496 44.806280 20.477800) '("rel[building]" "way[building]" "node[building]" "rel[highway]" "way[highway]" "node[highway]") "EPSG:3909")
  (get-data "https://www.overpass-api.de/api/" 60 '(44.804955 20.474496 44.806280 20.477800) '("rel[building]" "way[building]" "node[building]" "rel[highway]" "way[highway]" "node[highway]") "EPSG:3909")




  (flatten
    (list
      (create-flat-project 1 7 12 2)
      (create-flat-owner-history 2)
      (create-flat-representation-contexts 7)
      (create-flat-units-in-context 12)
      (create-flat-site 18 2 7 1)
      (create-flat-building 22 18 7 2 x)
      (create-flat-rel-aggregates 30 2 "1" "2" 18 [22])
      (create-flat-wall 31 21 7 2 coords "EPSG:3909" x)
      (create-flat-roof 50 21 7 2 coords "EPSG:3909" x)
      (create-flat-rel-contained-in-spatial-structure 58 2 "3" "4" (list 31 50) 26)))

  (filter #(clojure.set/subset? (set (:nodes %)) all-ids) buildings)
  (count (filter #(clojure.set/subset? (set (:nodes %)) all-ids) buildings))

  rel-members (flatten (map (fn [v] (map (fn [m] (:ref m)) v)) (map (fn [m] (:members m)) (filter #(and (= (:type %) "relation") (:building (:tags %))) (vals entities)))))

  )