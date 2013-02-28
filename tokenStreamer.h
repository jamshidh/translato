
#include <string>
#include <queue>
#include <iostream>
#include <map>

#include <boost/lexical_cast.hpp>
#include <boost/ptr_container/ptr_deque.hpp>
#include <boost/algorithm/string/replace.hpp>

using namespace std;

class Printable {

 public:
  virtual bool printIfPossible() = 0;

};

boost::ptr_deque<Printable> cachedStream;

map<string, string> variables;

class Var : public Printable {
 public:
  string name;
  string context;
  Var(string name, string context) { this->name = name; this->context = context; }
  bool printIfPossible();
};

bool Var::printIfPossible() {
  if (variables.find(context + ":" + name) == variables.end()) return false;
  cout << variables[context + ":" + name];
  return true;
}

class StringPrintable : public Printable {
 private:
  string theString;
 public:
  StringPrintable(string theString) {
    this->theString = theString;
  }
  bool printIfPossible();
};

bool StringPrintable::printIfPossible() {
  cout << theString;
  return true;
}

void printAsMuchAsPossible() {
  while(!cachedStream.empty()) {
    if (!cachedStream.front().printIfPossible()) break;
    cachedStream.pop_front();
 }
}

/*Convenience Wrappers*/

class BufferedOutput {
 public:
  const BufferedOutput &operator<<(const string &) const;
  const BufferedOutput &operator<<(Var *) const;
};

const BufferedOutput &BufferedOutput::operator<<(const string &s) const {
  cachedStream.push_back(new StringPrintable(s));
  printAsMuchAsPossible();
  return *this;
}

const BufferedOutput &BufferedOutput::operator<<(Var *v) const {
  cachedStream.push_back(v);
  printAsMuchAsPossible();
  return *this;
  return *this;
}

//This returns a unique value (per run) to label a variable context scope
char *getNewContext() {
  static int contextCount = 0;
  char *ret = (char *)malloc(20);
  if (snprintf(ret, 20, "%d", contextCount++) >= 20) {
    //This will never ever actually happen
    fprintf(stderr, "context too large\n");
    exit(-1);
  }

  return ret;
}

//Small convenience wrapper
Var *var(string name, string context) {
  return new Var(name, context);
}

//Small convenience wrapper
void setVar(string name, string value, string context) {

  if (variables.find(context + ":" + name) != variables.end()){
    if (variables[context+":"+name] != value){
      cerr << endl << "Error: the value of two attributes named " << name << " don't match each other" << endl << endl;
      exit(1);
    }
  }
  
  variables[context+":"+name] = value;
  printAsMuchAsPossible();
  return;
}

string escape(string s)
{
  string ret;

  using boost::algorithm::replace_all;
  replace_all(s, "&",  "&amp;");
  replace_all(s, "\"", "&quot;");
  replace_all(s, "\'", "&apos;");
  replace_all(s, "<",  "&lt;");
  replace_all(s, ">",  "&gt;");

  return s;
}
