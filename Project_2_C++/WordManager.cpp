#include "WordManager.h"
#include <iostream>   // for input/output (cout, cin)
#include <fstream>    // for file input (ifstream)
#include <vector>     // for dynamic array (vector)
#include <string>     // for using std::string
#include <cstdlib>    // for rand(), srand()
#include <ctime>      // for time() - used to seed randomness
#include <thread>

using namespace std;

/*
 * FUNCTION: getWordsByLength
 * ----------------------------------
 * Reads words from a newline-delimited text file (one word per line)
 * and returns a list (vector) of only the words that match a given length.
 *
 * PARAMETERS:
 * - filename: name of the text file to read from
 * - desiredLength: how long each word should be
 *
 * RETURNS:
 * - vector<string> of matching words
 */


 vector<string> getWordsByLength(const string& filename,const int desiredLength) {

    // vector<string> allwords;

    vector<string> matchingWords;

    ifstream file; //(filename);
    int count = 0;

    try {
        
        
        file.exceptions(ifstream::failbit | ifstream::badbit); // This makes ifstream throw if something goes wrong

        file.open(filename);

        string word;

        while ((count < 370105) && (getline(file, word)) ) { // need to figure out the file processing issue
            
            cout << "\r" << word << flush; // absolutely not needed but fun
            cout << "\033[3;0H";
            cout << "\033[K";

            //handle any carriage returns from windows files
            if (!word.empty() && word.back() == '\r'){
            word.pop_back(); }

            if (word.length() == desiredLength) {// checking desired length and inserting into returning list
                matchingWords.push_back(word);
            }

            count++;
        }


        cout << "processing finished" << endl;
        file.close();
        
    }
        
    catch(const ifstream::failure& e) {

            if (file.fail()) {

                throw runtime_error("Failed to read file: " + filename); // fail bit error

            } else if (file.bad()) {

                throw runtime_error("file failed to load due to integrity issue"); // bad bit errror
                
            } else {

                throw runtime_error("somthing else happened"); // uh oh

            }

            
            
        }



    return matchingWords;


 }

 string getRandomWord(const vector<string>& words) {
    srand(time(nullptr));

    int index = rand() % words.size();
    string selectedWord = words[index]; 

    this_thread::sleep_for(chrono::seconds(1));
    
    return selectedWord;
 }