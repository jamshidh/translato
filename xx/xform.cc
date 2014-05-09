

#include <fcntl.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xmlsave.h>
#include <stdio.h>
#include <string.h>

#include <map>
#include <string>

using namespace std;

#include "uniqueLabel.cc"

xmlDocPtr theDocument;

xmlDocPtr parseDoc(char *filename) {
  xmlDocPtr doc;
  int fd;

  if (strcmp(filename, "-") == 0) {
    fd = 0;
  }
  else {
    fd = open(filename, O_RDONLY);
    if (fd < 0) {
      perror("Failed to open file");
      exit(-1);
    }   
  }

  doc = xmlReadFd(fd, "noname.xml", NULL, 0);
  if (doc == NULL) {
    fprintf(stderr, "Failed to parse document\n");
    exit(-1);
  }
  
  return doc;
}

void outputDoc(xmlDocPtr doc) {
  xmlSaveCtxtPtr ctxt = xmlSaveToFd(1, "utf-8", 0);
  
  xmlSaveDoc(ctxt, doc);
  
  xmlSaveFlush(ctxt);
  xmlSaveClose(ctxt);
  
}

void usage() {
  fprintf(stderr, "Usage: xform FILENAME\n");
  exit(-1);
}

void printKeys(map<string, xmlNodePtr> theMap) {
  for(map<string,xmlNodePtr>::iterator it = theMap.begin(); it != theMap.end(); ++it) {
    printf("'%s'\n", it->first.c_str());
  }
}  

void trace(string x){
  printf("%s\n", x.c_str());
}

struct xformTemplate {
  const char **xpath;
  void (*action)(xmlDocPtr,map<string,xmlNodePtr>);
};

void matchCall(xmlDocPtr doc, string prefix, map<string, xmlNodePtr> vars, const char **xpath, xmlNodePtr n, void (*action)(xmlDocPtr,map<string,xmlNodePtr>)) {

  string fullXPath;
  if (prefix == "") fullXPath = xpath[0];
  else fullXPath = prefix + "/" + xpath[0];

  if (n->type != XML_ELEMENT_NODE) return; 

  if (strcmp((const char*)n->name, xpath[0]) != 0) return; 

  vars[fullXPath] = n;

  if (prefix == "") {
    vars["."] = n->parent;
    vars[string("/") + string((const char *) xmlDocGetRootElement(doc)->name)] = xmlDocGetRootElement(doc);
  }
  
  if (xpath[1] == NULL) {
    resetLabels();
    action(doc, vars);
    return;
  }
  else {
    xmlNodePtr child = n->children;
    while(child != n->last) {
      xmlNodePtr theNext = child->next;
      matchCall(doc, fullXPath, vars, xpath+1, child, action);
      child = theNext;
    }
  }

}

void applyTemplates(xmlDocPtr doc, xmlNodePtr n, xformTemplate **templates) {
    map<string, xmlNodePtr> vars;
    for(int i = 0; templates[i] != NULL; i++) {
      matchCall(doc, "", vars, templates[i]->xpath, n, templates[i]->action);
    }
}

void modifyNode(xmlDocPtr doc, xmlNodePtr node, xformTemplate **templates) {
  xmlNodePtr child = node->children;
  while(child != node->last) {
    modifyNode(doc, child, templates);
    xmlNodePtr theNext = child->next;
    applyTemplates(doc, child, templates);
    child = theNext;
  }
}

int readDocAndModify(int argc, char **argv, xformTemplate **templates) {
  if (argc != 2) usage();

  theDocument = parseDoc(argv[1]);

  xmlNodePtr root = xmlDocGetRootElement(theDocument);

  modifyNode(theDocument, root, templates);

  outputDoc(theDocument);

  xmlFreeDoc(theDocument);

  return 0;
}

#include "commands.cc"
#include "xform_g.cc"
