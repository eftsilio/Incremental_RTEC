static/areaIDs contains prolog files with the area ids and the areatype (Brest different from IMIS:  all areas are different except anchorage areas)

static/converted contains csv files containing the area ids with the area type (Brest)

static/patternsParameters/movingStatus.prolog contains the values of the moving fluent (Brest/IMIS)

static/patternsParameters/thresholds.prolog contains the thresholds for the patterns (Brest/IMIS)

static/portRelatedData/portStatus.prolog contains the port status values (Brest/IMIS)

static/vesselInformation/typeSpeeds.prolog contains min max and avg service speed per vessel type (Brest/IMIS)

static/vesselInformation/vesselPairs.prolog contains the vessel pairs (useful when dynamic grounding isn't used) (Brest)

static/vesselInformation/vessels.prolog contains the vessel mmsi codes (useful when dynamic grounding isn't used) (Brest)

static/vesselInformation/vesselStaticInfo.prolog contains static information for the vessels of the dataset (mmsi,type,draught) (Brest different from IMIS: AIS static data is different between the datasets also in the IMIS dataset draught is not available)

static/loadStaticData.prolog loads static data (Brest/IMIS)

static/staticDataPredicates.prolog contains predicates are used inside the patterns (Brest/IMIS)
