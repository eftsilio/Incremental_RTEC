# Application

Event recognition for [Maritime Situational Awareness](http://cer.iit.demokritos.gr/blog/applications/maritime_surveillance/) in the area of Brest (France).

# File Description

- ./patterns_compiled_inc.prolog: The composite event definitions.
- ./declarations.prolog: The declarations file.
- ./mass-queries.prolog: The application execution file. When recognition finishes, it outputs the recognized composite events along with various statistics concerning recognition.
- ./data/dynamic_data/Brest_dataset_download.txt: A link to download synthetic versions of the original Brest dataset. We provide five versions where each time 5%, 10%, 20%, 40% and 80% of the input events have been delayed. Note that the datasets have to be placed in the data/dynamic_data folder.
- ./data/loadFiles.prolog: A prolog file that loads all the necessary data.
- ./data/static_data/loadStaticData.prolog: A prolog file that loads all the necessary static data. 
- The areaIDs used in recognition in prolog format: ./data/static_data/areaIDs
- Vessel static information such as speed per vessel type and vessel types:
	- data/static_data/vesselInformation/typeSpeeds.prolog
	- data/static_data/vesselInformation/vesselStaticInfo.prolog
- ./utils/compare.prolog: Auxiliary functions.
- ./utils/dynamic-grounding.prolog: This file is responsible for grounding only the necessary vessel entities in each temporal window.
- ./utils/globals.prolog: Initialization of global variables.
