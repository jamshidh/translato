
#include <sstream>

map<string, string> labelMap;

int suffixCounter = 0;

void resetLabels() {
  labelMap.clear();
}

string u(string x) {
  if (labelMap.count(x) == 1) return labelMap[x];

  stringstream ret;

  ret << x << string("_") << suffixCounter++;
  labelMap[x] = ret.str();
  return ret.str();
}
