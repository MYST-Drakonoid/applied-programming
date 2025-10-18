#include <iostream>
#include <vector>
#include <string>
#include "WordManager.h"
#include <filesystem>
using namespace std;


int main() {
    int length;
    bool run = true;
    string selectedWord;
    vector<string> wordlist;

    cout << "WELCOME TO HANGMAN" << endl;
    try {
            cout << "Enter desired word length: ";
            cin >> length;

            // Check if input failed (e.g., user entered letters or negative numbers)
            if (cin.fail()) {
                throw runtime_error("Invalid input! Please enter a number.");
            }


            if (length <= 0) {
                throw runtime_error("Length must be greater than zero.");
            }



    } catch (const runtime_error& e) {
            cerr << "Error: " << e.what() << endl;
            return 1; // stop program
        }
    // filesystem::path exeDir = filesystem::current_path();

    // filesystem::path filePath = exeDir / "words.txt";


    try {
        wordlist = getWordsByLength("words.txt", length);

    } catch (const runtime_error& e) {
                    int end;
            cerr << "File Error: " << e.what() << endl;
            cin >> end;          
    }
    
    if (wordlist.empty()) {
        cerr << "no words found of length: " << length << endl;
        return 0;
    } else {
        selectedWord = getRandomWord(wordlist);
    }

    hangmanGame(selectedWord);





}

