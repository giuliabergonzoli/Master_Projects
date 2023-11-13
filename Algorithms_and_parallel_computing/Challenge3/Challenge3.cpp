#include <mpi.h>
#include <fstream>
#include <iostream>
#include <limits> //optional helper header
#include "Matrix.h"

double matrix_max(const Matrix & M){
	//--------YOUR matrix_max CODE BEGINS HERE (optional helper function)---------
	// No std::cout can be used in the code you will upload
    double maxM = M.getElem(0,0);
    for (std::size_t i = 0; i < M.getNrows(); ++i) {
        for (std::size_t j = 0; j < M.getNcols(); ++j) {
            if(M.getElem(i,j)>maxM)
                maxM = M.getElem(i,j);
        }
    }
    return maxM;
	//---------YOUR matrix_max CODE ENDS HERE (optional helper function)----------
}

int main(int argc, char *argv[]) {

	//Init
	MPI_Init (&argc, &argv);
	int rank (0), size (0);
	MPI_Comm_rank (MPI_COMM_WORLD, &rank);
	MPI_Comm_size (MPI_COMM_WORLD, &size);

	//Read from file
	Matrix M(0,0), result(0,0);
	if (rank == 0){
		std::string filename = argv[1];
		std::ifstream ifs(filename);
		M.read(ifs);
		M.getMajor() == "row-major" ? result.setDim(size,1) : result.setDim(1,size);
	}

	//Max Pooling
	//---------YOUR MAX POOLING CODE BEGINS HERE------------------
	// No std::cout can be used in the code you will upload
    unsigned n(0);
    if(rank==0){
        n = M.getNcols();
    }

    MPI_Bcast (&n, 1, MPI_UNSIGNED, 0, MPI_COMM_WORLD);

    const unsigned stripe = n/size;
    Matrix local_M(stripe,n);
    MPI_Scatter(M.data(), stripe*n, MPI_DOUBLE, local_M.data(), stripe*n, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    double local_maxM = matrix_max(local_M);
    MPI_Gather(&local_maxM, 1, MPI_DOUBLE, result.data(), 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);

	//---------YOUR MAX POOLING CODE ENDS HERE---------------------

	//Output
	if(rank==0){
		// These are the only 2 std::cout allowed
		std::cout<<"Input matrix is:"<<std::endl;
		M.print()<<std::endl;
		std::cout<<"Max pooling gives: "<<std::endl;
		result.print();
	}

	//Finalize
	MPI_Finalize ();

	return 0;
}

