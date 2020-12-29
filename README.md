# Incremental RTEC: Incremental Run-Time Event Calculus

[RTEC](https://github.com/aartikis/RTEC) is an open-source [Event Calculus](https://en.wikipedia.org/wiki/Event_calculus) dialect optimised for data stream reasoning. Incremental RTEC is an extension of RTEC for handling more efficiently out-of-order data streams. It is written in Prolog and has been tested under [YAP 6.2](https://en.wikipedia.org/wiki/YAP_(Prolog)).

# File Description

To run Incremental RTEC you need first to download the dataset of an application. In the /examples directory there are folders each one corresponding to a different application. Each application folder contains patterns and datasets for experimentation. In the data/dynamic_data directory of each application you can find instructions for downloading a dataset. For now, we provide only the data for the application "maritime_brest".

After you have downloaded a dataset you can run Incremental RTEC from the command line. Assuming that you have downloaded the datasets for the application "maritime_brest", please type in the command line:

```
$ yap -s 0 -h 0 -t 0 -l ./examples/maritime_brest/data/loadFiles.prolog -g "performFullER(['./examples/maritime_brest/data/dynamic_data/delayed_5%.csv'],'./examples/maritime_brest/data/results/stats.txt', './examples/maritime_brest/data/results/CEs.txt', 14673763, 7200, 3600, 14513475), halt."
```

Next, we explain the arguments given in the above command.

- "./examples/maritime_brest/data/loadFiles.prolog" is a prolog file that loads all the necesary files for the execution of Incremental RTEC. It can be found in each application folder under the *data* directory.
- Arguments of *performFullER* predicate:
    - "['./examples/maritime_brest/data/dynamic_data/delayed_5%.csv']" is the path to the dataset.
    - "./examples/maritime_brest/data/results/stats.txt'" is the path where the file with various statistics concerning recognition will be saved. In each application folder you can find a *data/results* directory for saving the results of the experiments.
    - "'./examples/maritime_brest/data/results/CEs.txt'" is the path where the file with the produced composite events will be saved. In each application folder you can find a *data/results* directory for saving the results of the experiments.
    - "14673763" is the starting time of the recognition. In each application folder there is an *instructions* file where the start and end time for the specific application are documented.
    - "7200" is the size of the sliding window.
    - "3600" is the size of the sliding step.
    - "14513475" is the ending time of the recognition.