# Algorithm_and_Parallel_Computing
Repository for the projects of the course of Algorithm and Parallel Computing - Polimi 2022/2023

The course included three challenges:

1.	You have to implement a class to support matrices and allow the computation of determinant.

2.	Your task is to implement two methods, "hstack" and “join", declared as follows:
	DataFrame hstack(DataFrame &otherDataFrame);
	DataFrame join(DataFrame &otherDataFrame, string onMyCol, string onColOfOther);
	First, the hstack method will "H" orizontally pack the two DataFrames similar to the equivalent	Python function.
	The second task at hand is to implement the “join” operator; join works as the same usual command in SQL,
	namely by executing a Cartesian product on tables and filtering rows out based on a criterion (logic expression) between values in different columns.

3.	You have to implement a parallel program inspired to max pooling method for square matrices;
	you can rely on a provided Matrix class. Your implementation has to take into account whether given matrices are row-major or column-major.