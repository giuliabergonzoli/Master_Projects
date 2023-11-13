//
//  Matrix.h
//  Determinant2
//
//  Created by ing.conti on 07/10/22.
//

#ifndef MATRIX_H
#define MATRIX_H

#include <vector>
#include <cmath>

#include <limits>

constexpr double INVALID{ std::numeric_limits<double>::quiet_NaN() };


// DO NOT ALTER Class name.
class Matrix{

private:
    // “YOUR CODE HERE” if needed
    std::vector<std::vector<double>> original;

public:
    Matrix( size_t nRows, size_t nCols); // zeros matrix, only to allow run
    double getElem(size_t i, size_t j) const;
    double determinant() const;
    Matrix delete_matr(size_t c) const;

    // “YOUR CODE HERE”
    // write here the constructor that allow to call:
    // Matrix m1 = Matrix( {{1, 2}, {3,4}}  );
    Matrix(const std::vector<std::vector<double>> &original);
};


#endif //MATRIX_H
