//
//  main.cpp
//  Determinant2
//
//  Created by ing.conti on 07/10/22.
//

#include <iostream>
#include "Matrix.h"

int main(int argc, const char * argv[]) {
    
    // see this file for declaration,
#include "TestValues.h"

    double det = m1.determinant();
    std::cout << det << std::endl;

    return 0;
}
