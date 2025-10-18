#ifndef WORDMANAGER_H
#define WORDMANAGER_H

#include <string>
#include <vector>

std::vector<std::string> getWordsByLength(const std::string& filename, const int desiredLength);
std::string getRandomWord(const std::vector<std::string>& words);

void hangmanGame(const std::string& selectedWord);

#endif
