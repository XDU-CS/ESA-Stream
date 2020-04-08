# ESA-Stream

## Introduction
**ESA-Stream**(**E**fficient **S**elf-**A**daptive **Stream** algorithm), is a fully online and light-weight data stream clustering framework. In summary, the main contributions of this algorithm are:
 - It presents a fully online data stream clustering method ESA-Stream, which does not need the traditional time-consuming offline stage and can detect arbitrarily shaped clusters efficiently and effectively
 - It proposes an efficient self-adaptive technique to learn parameter settings dynamically during clustering to keep up with the drifting or evolving data streams. We are not aware of any existing work that is able to do this.
 - It proposes the notions of the grid density centroid and grid feature vector to speedup dimensionality reduction and to cluster data streams in an efficient and fully online manner.

The purposed ESA-Stream framework is outlined in Fig. 1. With five components cooperating with each other, ESA-Stream can cluster online real-time and high-dimensional evolving data streams efficiently and effectively. The functions of the five components are described as follows.
![Fig. 1. Stream clustering pipeline of ESA-Stream.](fig/fig1.png?v=1&type=image)
<center>Fig. 1. Stream clustering pipeline of ESA-Stream.</center>


 - **Data Stream Acceptor**: It accepts each data point from a stream in real-time, and performs a min-max normalization for each dimension of the data point.
 - **Grid Manager**: It first maps or projects the normalized data point to a grid, then calculates and updates the density and feature vector of the grid online at a time interval $gap$ between two consecutive clustering updates. It then produces a grid density centroid data streams, which is used to reduce the dimensionality efficiently(if needed). Since the centroid data streams is much smaller than the original data, the dimensionality reduction (using PCA) can be done very efficiently.
 - **Parameters Online Learner**: It keeps on learning and updating the parameters online when new data points in the stream arrive.
 - **Parallel Clustering Engine**: After each $gap$, it intelligently clusters the grid density centroid data (not the original data points) from *Grid Manager* in parallel with the strategy ECS. It then generates the intermediate clustering results.
 - **Result Integrator**: It integrates the intermediate clustering results and outputs the final clusters for $gap$.



## Install & Running
###  Environment
Please install JDK-1.8+ and Scala-2.11+ before running the program.

### Dependencies
```
akka-actor_2.11-2.4.7.jar
akka-kernel_2.11-2.4.7.jar
arpack_combined_all-0.1.jar
breeze-macros_2.11-0.13.2.jar
breeze_2.11-0.13.2.jar
breeze_2.12-0.13.2.jar
config-1.3.1.jar
core-1.1.2.jar
lombok-1.12.2.jar
shapeless_2.11-2.3.2.jar
```
The above jar files have been saved in "./lib/".

### Running
In the terminal, you can use the following command to run the program.
```
java -jar ESA-Stream.jar /path/to/your/file [dimension] [length] [upbound]
```
Program arguments: 
 - **"/path/to/your/file"**: the file path of data stream, string value;
 - **[dimension]**: the dimension of the data stream, integer value;
 - **[length]**: the length of the grid, double value;
 - **[upbound]**: the numerical upper bound of attributes in the data stream, double value;

For example
```
java -jar ESA-Stream.jar data/2dpoints.txt 2 0.02 1
```





