# Snake-Game-Assembly

# Installtion Instruction:
For this Project, you need compiler for Java to run this file. There are many compilers like Netbeans, Eclipse and many others which support java. 
* Netbeans: https://netbeans.apache.org/ 
* Eclipse: https://www.eclipse.org/downloads/

# Compilation Instruction:
Just Install the compiler from their official website and Run project. No such Dependency Required for this Project explicitly.

# Working:
In this programming assignment, genetic algorithm combined with local search was used to solve the scheduling problem. A genetic algorithm is a search heuristic that is inspired by Charles Darwin’s theory of natural evolution. This algorithm reflects the process of natural selection where the fittest individuals are selected for reproduction in order to produce offspring of the next generation.

There were five phases that were used to compute results.
* Initial population
*	Fitness function
*	Selection
*	Crossover
*	Mutation
*	Local Search

# Technique Used:

* Population:
In the file general.info there were three days and seven slots per day given to us, so we made 2d array of [3][7] and at every index of it there was an array list representing the courses ids whose exams were to be held. That was our single chromosome which was populated using random numbers.


# Approach:

1.	In case number 1 mention in above points, we have set the limit to by making of array of given number of days in GeneralInfo.txt and populate it accordingly and we make sure it that each and every exam must be scheduled in given days.
2.	In case number 2, as I discuss above our approach to solve this problem. We basically make List type array which contain students of given courses List at given time slot we find the count of total student and compare the room capacity with it.
3.	In case 3 and case 5 we basically use same approach. We use same technique as mention in point 2, get the list of students of each course in given slot and then simply compare it.
4.	In case 7, we simply count the student id count in given day, if it exceeds then 3, we simply assume that this particular student have three exams in given day.
 

All these points which are mentioned above are taken into consideration while computing fitness of chromosome.

