# fetch_data

fetch data: (This simple code just downloads miniseed from IRIS)  
            executable file:"fetch_data" in directory "fetch_data"
            source file    :"fetch_data.f90", comple with make. This code is used to download daily mseed data.
                              Actually, with a little modification this code can be used to donwload earthquake data., e.g. fetch_eq.f90
              how to run     :a parameter file is needed with the form
                              "station.list
                              year_begin day_begin year_end day_end
                              component number_of_component download_response_alone_or_not(1/0)"
                              the station.list is like:
                              TA V35A
                              XX XXXX
                              See the example: for_fetch
                              The miniseed is in "seed", response file in "resp", station information in "mdat".
              attention      :this code just call "FetchData" which is from IRIS website, so please download this FetchData first, then
                              put it under the enviornmental path and make it executable. Mostly, make sure your computer is connecting to the internet with high speed^_^
                              BTW, FetchData is written using perl, so make sure you have the newest version of perl. Maybe you wound have some bugs when running FetchData,
                              just download the packages that required (in the bug information).
