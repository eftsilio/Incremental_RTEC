# Incremental RTEC: Incremental Run-Time Event Calculus

[RTEC](https://github.com/aartikis/RTEC) is an open-source [Event Calculus](https://en.wikipedia.org/wiki/Event_calculus) dialect optimised for data stream reasoning. Incremental RTEC is an extension of RTEC for handling more efficiently out-of-order data streams. It is written in Prolog and has been tested under [YAP 6.2](https://en.wikipedia.org/wiki/YAP_(Prolog)).

# License

Incremental RTEC comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; see the [GNU Lesser General Public License v3 for more details](http://www.gnu.org/licenses/lgpl-3.0.html).

# File Description

To run Incremental RTEC you need first to download the dataset of an application. In the /examples directory there are folders each one corresponding to a different application. Each application folder contains patterns and datasets for experimentation as well as a readme file. In the data/dynamic_data directory of each application you can find instructions for downloading a dataset. For now, we provide only the data for the application "maritime_brest".

After you have downloaded a dataset you can run Incremental RTEC from the command line using the *run-exps.py* file. You will need to have installed Python3 in your system. Assuming that you have downloaded the datasets for the application "maritime_brest", please type in the command line:

```python
$ python3 run-exps.py
```

and then follow the on-screen instructions in order to select the desired parameters for recognition. When recognition finishes, the produced composite events and various statistics concerning recognition will be saved in the *data/results* directory of the application. If the directory does not exist, it will be created.

# Documentation

- Tsilionis E., Artikis A. and Paliouras G., [Incremental Event Calculus for Run-Time Reasoning](http://cer.iit.demokritos.gr/publications/papers/2019/EfthimisTsilionis.pdf). In 13th International Conference on Distributed and Event-Based Systems (DEBS), pp. 79–90, 2019.
- Tsilionis E., Koutroumanis N., Nikitopoulos P., Doulkeridis C. and Artikis A., [Online Event Recognition from Moving Vehicles](http://cer.iit.demokritos.gr/publications/papers/2019/Tsilionis_ICLP19.pdf). In Theory and Practice of Logic Programming (TPLP), 19(5-6), pp. 841–856, 2019. 

