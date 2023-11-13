//
//  Matrix.cpp
//  Determinant2
//
//  Created by ing.conti on 07/10/22.
//

#include <vector>
#include <iostream>

#include "Matrix.h"

// “YOUR CODE HERE” for constructor.
Matrix::Matrix(const std::vector<std::vector<double>> &original) : original(original) {}


Matrix::Matrix( size_t nRows, size_t nCols) {
    // “YOUR CODE HERE”
    std::vector <double> vec(nCols, 0);
    for (size_t i = 0; i < nRows; ++i) {
        original.push_back(vec);
    }
}


double Matrix::getElem(size_t i, size_t j) const{
    // “YOUR CODE HERE”
    size_t nRows = original.size();
    size_t nCols = original[0].size();
    if (i<nRows && j<nCols){
        return original.at(i).at(j);
    }
    return INVALID; // default value. Only to allow build.
}


Matrix Matrix::delete_matr(size_t col) const{
    std::vector<std::vector<double>> submatr;
    size_t nRows = original.size();
    size_t nCols = original[0].size();

    for (size_t i = 1; i < nRows; ++i) {
        std::vector<double> vec;
        for (size_t j = 0; j < nCols; ++j) {
            if (j!=col){
                vec.push_back(getElem(i,j));
            }
        }
        submatr.push_back(vec);
    }
    return submatr;
}

double Matrix::determinant() const{
    // “YOUR CODE HERE”
    double det = 0;
    size_t nRows = original.size();
    size_t nCols = original[0].size();

    if (nRows==nCols){
        if (nRows==2 && nCols==2)
            return original.at(0).at(0)*original.at(1).at(1) - original.at(0).at(1)*original.at(1).at(0);
        for (size_t j = 0; j < nCols; ++j) {
            Matrix newmatr = delete_matr(j);
            det += getElem(0,j)*pow(-1,1+j+1)*newmatr.determinant();
        }
        return det;
    }
    return INVALID; // default value. Only to allow build.
}


