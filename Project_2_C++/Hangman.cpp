#include "WordManager.h"
#include <iostream>   // for input/output (cout, cin)
#include <vector>     // for dynamic array (vector)
#include <string>     // for using std::string
#include <thread>
using namespace std;

void displayHangman(int wrongGuesses) { //displays the hangman

        const vector<string> hangmanStages = {
        "  +---+\n  |   |\n      |\n      |\n      |\n     ===",
        "  +---+\n  |   |\n  O   |\n      |\n      |\n     ===",
        "  +---+\n  |   |\n  O   |\n  |   |\n      |\n     ===",
        "  +---+\n  |   |\n  O   |\n /|   |\n      |\n     ===",
        "  +---+\n  |   |\n  O   |\n /|\\  |\n      |\n     ===",
        "  +---+\n  |   |\n  O   |\n /|\\  |\n /    |\n     ===",
        "  +---+\n  |   |\n  O   |\n /|\\  |\n / \\  |\n     ==="
        };
        if (wrongGuesses < 0) wrongGuesses = 0;
        if (wrongGuesses >= hangmanStages.size()) wrongGuesses = hangmanStages.size() - 1;
        std::cout << hangmanStages[wrongGuesses] << endl;
    }


void clearConsole() { //clears the console for consistant interface
    #ifdef _WIN32
        system("cls");       // Windows
    #elif defined(__linux__) || defined(__APPLE__)
        system("clear");     // Linux or macOS
    #else
        std::cout << std::string(50, '\n'); // Fallback if unknown OS
    #endif
}    

char getCharInput() { //grabs input
    string input;

    while (true) {
        try {
            cout << "Enter your guess: ";
            cin >> input;

            if (input.length() != 1) {
                throw runtime_error("Invalid input: please enter exactly one character.");
            }

            char c = input[0];
            return c; // valid char
        }
        catch (const runtime_error& e) {
            cerr << e.what() << endl;

            // Clear input stream in case of bad state
            cin.clear();
            cin.ignore(numeric_limits<streamsize>::max(), '\n');
        }
    }
}

void hangmanGame(const string& selectedWord) { // actually runs the gmae


    bool game = true;
    bool win = false;
    int guesses = 0;
    char guess;
    int wrongGuesses = 0;
    struct Letter {
        char character;
        bool guessed;
    };

    vector<Letter> letters;

    for (char c : selectedWord) {
        letters.push_back({c, false});
    }

    while (game == true) {
        clearConsole();

        displayHangman(wrongGuesses);

        for (const Letter& letter : letters) {
            if (letter.guessed)
                {cout << letter.character << " ";}
            else
                {cout << "_ ";}
        }

        guess = getCharInput();


bool correct = false;

for (Letter& letter : letters) { // checking guess for correctness
    if (guess == letter.character) {
        letter.guessed = true;
        correct = true; 
    }
}

if (!correct) {
    wrongGuesses++; // increment only once
}else {
    guesses++;
}


        if (wrongGuesses >= letters.size()) { //terminate the game if guess numbers reacha threshhold
            game = false;
        }else if (guesses >= letters.size())
        {
            win = true;
            game = false;
        }
        

    }

    if (win) {
        cout << "YOU WIN!!!!!!" << endl;
    } else {
        cout << "GAME OVER" << endl;
    }

}  

