// DataFrame.cpp source file

#include <iomanip>
#include "DataFrame.h"

std::string DataFrame::PREFIX1{"df1_"};
std::string DataFrame::PREFIX2{"df2_"};


// Helper functions Protos:
void tabbed(double value, bool showZeros = true);
void tabbed(std::string string);
void tab();
std::vector<std::string> addPrefix(const std::vector<std::string> & to, std::string prefix);




DataFrame::DataFrame(const std::string &columnNamesString) : DataFrame() {
    Keys columnsNames = split(columnNamesString, ' ');

    for (const Key &name: columnsNames)
        dataFrameData[name];
}


DataFrame::Keys DataFrame::split(const std::string &s, char delim) const {
    Key word;
    Keys v;
    std::istringstream columns(s);

    while (std::getline(columns, word, delim)) v.push_back(word);

    return v;
}

bool DataFrame::checkColumnName(const std::string &columnName) const {
    return dataFrameData.find(columnName) != dataFrameData.end();
}


Column DataFrame::getColumn(const std::string &columnName) const {
    if (checkColumnName(columnName))
        return dataFrameData.at(columnName);
    else {
        std::cerr << "Error, " << columnName << " is unknown" << std::endl;
        return Column();
    }
}


void DataFrame::setColumn(const std::string &columnName,
                           const Column &column) {
    if (checkColumnName(columnName))
        dataFrameData[columnName] = column;
    else
        std::cerr << "Error, " << columnName << " is unknown" << std::endl;
}



bool DataFrame::addColumn(const Key &columnName,
                           const Column &columnData) {
    if (!checkColumnName(columnName)) {
        dataFrameData[columnName] = columnData;
        return true;
    }
    else {
        return false;
    }
}



std::vector<DataFrame::Key> DataFrame::getColumnNames() const {

    Keys names = Keys();

    for (const valueType &element: dataFrameData) {
        std::string s = element.first;
        names.push_back(s);
    }

    return names;
}


// // we se are simulating arrays, number or rows will be the max value in keys
dimension DataFrame::getDimension() const {
    auto colNames = getColumnNames();
    if (colNames.empty()){
        return {0,0};
    }

    auto cols = colNames.size();

    // cannot be equal, se we have sparse columns:

    /* if dense, every column with same height, so:
    auto values0 = get_column(colNames[0]);
    auto rows = values0.height();
     */
    
    // but we must search for max key:
    size_t rows = 0;
    for(std::string name: colNames){
        auto values = getColumn(name);
        auto H = values.height();
        rows = std::max(rows, H);
    }
    
    return {rows, cols};
}



void DataFrame::print(void) const {

    // write all columns names:
    auto colNames = getColumnNames();
    for (auto t: colNames) {
        tabbed(t);
    }

    newLine();

    auto dimension = getDimension();
    
    if (dimension.rows == 0)
        return;

    for (int row = 0; row < dimension.rows; row++) {

        auto valuesArray = getValuesAtRow(row);
            for(auto v : valuesArray){
                tabbed(v);
            }
            newLine();
    }

    newLine();
}


// TO BE IMPLEMENTED:

DataFrame
DataFrame::hstack(DataFrame &otherDataFrame){
    // YOUR CODE HERE
    // only to allow RUN: (please write your own code)
    Keys colnames1 = getColumnNames();
    Keys colnames2 = otherDataFrame.getColumnNames();

    std::vector<std::string> newcolnames1 = addPrefix(colnames1, PREFIX1);
    std::vector<std::string> newcolnames = newcolnames1;
    std::vector<std::string> newcolnames2 = addPrefix(colnames2, PREFIX2);
    newcolnames.insert(newcolnames.end(),newcolnames2.begin(),newcolnames2.end());
    DataFrame DataMerge = DataFrame(newcolnames);

    size_t i=0;
    for (std::string &name: colnames1) {
        Column col = getColumn(name);
        DataMerge.setColumn(newcolnames[i],col);
        i++;
    }

    for (std::string &name: colnames2) {
        Column col = otherDataFrame.getColumn(name);
        DataMerge.setColumn(newcolnames[i],col);
        i++;
    }

    return DataMerge;

}

DataFrame DataFrame::join(DataFrame &otherDataFrame, std::string onMyCol, std::string onColOfOther){
    // YOUR CODE HERE
    // only to allow RUN: (please write your own code)
    Column colMy = getColumn(onMyCol);
    Column colOther = otherDataFrame.getColumn(onColOfOther);
    std::vector<size_t> idx1;
    std::vector<size_t> idx2;

    for (auto v2 : colOther.values) {
        Value val2 = v2.second;
        for (auto v1 : colMy.values) {
            Value val1 = v1.second;
            if (val1==val2){
                idx1.push_back(v1.first);
                idx2.push_back(v2.first);
            }
        }
    }

    DataFrame Data1 = DataFrame(getColumnNames());
    DataFrame Data2 = DataFrame(otherDataFrame.getColumnNames());

    for (size_t i : idx1) {
        ValuesArray l1 = getValuesAtRow(i);
        Data1.addRow(l1);
    }

    for (size_t i : idx2) {
        ValuesArray l2 = otherDataFrame.getValuesAtRow(i);
        Data2.addRow(l2);
    }

    return Data1.hstack(Data2);
}

void DataFrame::addRow(ValuesArray l){
    auto iter = dataFrameData.begin();
    size_t i = 0;
    while (iter != dataFrameData.end()){
        iter->second.insert(l[i], iter->second.height());
        iter++;
        i++;
    }
}


// ancillary code

DataFrame::DataFrame(std::vector<std::string> columnsNames)
: DataFrame() {
    for (const Key &name: columnsNames)
        dataFrameData[name];
}


ValuesArray DataFrame::getValuesAtRow(size_t row) const{
    ValuesArray result = {};
    
    auto colNames = getColumnNames();
    for (auto name:colNames) {
        auto column = getColumn(name);
        auto value = column.values[row];
        result.push_back(value);
    }
    return result;
}




// MARK: Helper functions:



void tabbed(double value, bool showZeros){

    if (value == 0 && !showZeros){
        std::cout << "       " << "|";
    }else{
        std::cout << std::setw(7);
        std::cout << value << "|";
    }
}



void tabbed(std::string string){
    auto stdLen = 7;
    auto len = string.size();
    auto spaces = stdLen-len;
    std::cout << string;
    for (int i = 0; i < spaces; ++i) {
        std::cout << " ";
    }
    std::cout << "|";

}

void tab(){
    tabbed("");
}

void newLine(std::string s){
    std::cout << s << std::endl;
}



std::vector<std::string> addPrefix(const std::vector<std::string> & to, std::string prefix){
    std::vector<std::string> out;

    std::transform(to.begin(), to.end(),
                   std::back_inserter(out),
                   [&prefix](std::string v) { return prefix+v; }
    );
    return out;
}

