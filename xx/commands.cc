#include <libxml/xpath.h>

#include <vector>

inline void checkExistence(map<string, xmlNodePtr> &vars, xmlNodePtr n, const char *varName, const char *funcName) {
  if (n == NULL) { 
    printf("error: '%s' is NULL in call to %s\n", varName, funcName);
    printf("Known variables:\n");
    printKeys(vars);
    exit(-1);
  }
}

vector<xmlNodePtr> getXPathNodes(xmlDocPtr doc, xmlNodePtr n, const char *xpath) {
    xmlXPathObjectPtr xpathObj; 
    xmlXPathContextPtr xpathCtx; 
    
    xpathCtx = xmlXPathNewContext(doc);
    if(xpathCtx == NULL) {
        fprintf(stderr,"Error: unable to create new XPath context\n");
        exit(-1);
    }
    
    xpathCtx->node = n;

    xpathObj = xmlXPathEvalExpression(BAD_CAST xpath, xpathCtx);
    if(xpathObj == NULL) {
        fprintf(stderr,"Error: unable to evaluate xpath expression\n");
        exit(-1);
    }

    vector<xmlNodePtr> ret;

    int size = (xpathObj->nodesetval) ? xpathObj->nodesetval->nodeNr : 0;
    
    for(int i = 0; i < size; ++i) {
      ret.push_back(xpathObj->nodesetval->nodeTab[i]);
    }

    xmlXPathFreeObject(xpathObj);
    xmlXPathFreeContext(xpathCtx); 
    
    return ret;
}

xmlNodePtr getXPathNode(xmlDocPtr doc, xmlNodePtr n, const char *xpath) {
    xmlXPathObjectPtr xpathObj; 
    xmlXPathContextPtr xpathCtx; 

    xpathCtx = xmlXPathNewContext(doc);
    if(xpathCtx == NULL) {
        fprintf(stderr,"Error: unable to create new XPath context\n");
        xmlFreeDoc(doc); 
        exit(-1);
    }
    
    xpathCtx->node = n;

    xpathObj = xmlXPathEvalExpression(BAD_CAST xpath, xpathCtx);
    if(xpathObj->nodesetval == NULL) {
        fprintf(stderr,"Error: unable to evaluate xpath expression\n");
        exit(-1);
    }
    
    if(!xpathObj->nodesetval) {
        fprintf(stderr,"nodesetval is NULL\n");
        exit(-1);
    }

    if(xpathObj->nodesetval->nodeNr != 1) {
      fprintf(stderr,"xpath returned %d values for '%s'\n", xpathObj->nodesetval->nodeNr, xpath);
      exit(-1);
    }

    xmlNodePtr ret = xpathObj->nodesetval->nodeTab[0];

    xmlXPathFreeObject(xpathObj);
    xmlXPathFreeContext(xpathCtx); 
    
    return ret;
}



xmlNodePtr getNode(xmlDocPtr doc, map<string, xmlNodePtr> &vars, const char *name, const char *funcName) {
  xmlNodePtr val = vars[string(name)];
  //checkExistence(vars, val, name, funcName);

  if (val == NULL) val = getXPathNode(doc, vars["."], name);

  return val;
}

vector<xmlNodePtr> getNodes(xmlDocPtr doc, map<string, xmlNodePtr> &vars, const char *name, const char *funcName) {
  //TODO- use the vars map to prefixes

  return getXPathNodes(doc, vars["."], name);
}


void moveBefore(xmlDocPtr doc, map<string, xmlNodePtr> &vars, const char *fromXpath, const char *toXpath) {
  vector<xmlNodePtr> fromNodes = getNodes(doc, vars, fromXpath, "moveBefore");
  xmlNodePtr to = getNode(doc, vars, toXpath, "moveBefore");
  
  for(vector<xmlNodePtr>::iterator it = fromNodes.begin(); it != fromNodes.end(); ++it) {
    xmlUnlinkNode(*it);
    xmlAddPrevSibling(to, *it);
    
  }

}

void moveLast(xmlDocPtr doc, map<string, xmlNodePtr> &vars, const char *fromXpath, const char *toXpath) {
  vector<xmlNodePtr> fromNodes = getNodes(doc, vars, fromXpath, "moveBefore");
  xmlNodePtr to = getNode(doc, vars, toXpath, "moveBefore");
  
  for(vector<xmlNodePtr>::iterator it = fromNodes.begin(); it != fromNodes.end(); ++it) {
    xmlUnlinkNode(*it);
    xmlAddChild(to, *it);
    
  }
}

void addFirst(xmlNodePtr to, xmlNodePtr n) {
  if (to->children) xmlAddPrevSibling(to->children, n);
  else xmlAddChild(to, n);
}

void moveFirst(xmlDocPtr doc, map<string, xmlNodePtr> &vars, const char *fromXpath, const char *toXpath) {
  vector<xmlNodePtr> fromNodes = getNodes(doc, vars, fromXpath, "moveBefore");
  xmlNodePtr to = getNode(doc, vars, toXpath, "moveBefore");
  
  for(vector<xmlNodePtr>::iterator it = fromNodes.begin(); it != fromNodes.end(); ++it) {
    xmlUnlinkNode(*it);
    addFirst(to, *it);
  }

}

void deleteNode(xmlDocPtr doc, map<string, xmlNodePtr> &vars, const char *xpath) {
  xmlNodePtr n = getNode(doc, vars, xpath, "deleteNode");
  xmlUnlinkNode(n);
  xmlFreeNode(n);
}




void addElement(xmlDocPtr doc, map<string, xmlNodePtr> &vars, const char *xpath, const char *tagname) {
  xmlNodePtr n = getNode(doc, vars, xpath, "addElement");
  xmlNodePtr newNode = xmlNewNode(n->ns, BAD_CAST(tagname));
  xmlAddChild(n, newNode);
  vars[string(xpath) + "/" + string(tagname)] = newNode;
}

void addAttribute(xmlDocPtr doc, map<string, xmlNodePtr> &vars, const char *xpath, const char *name, const char *value) {
  xmlNodePtr n = getNode(doc, vars, xpath, "addAttribute");
  xmlNewProp(n, BAD_CAST(name), BAD_CAST(value));
}
