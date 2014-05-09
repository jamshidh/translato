

void action1(xmlDocPtr doc, map<string, xmlNodePtr> vars) {
  
    xmlNodePtr elem0 = getNode(doc, vars, "lambda", "action");
  
    xmlNodePtr elem0_1 = xmlNewNode(elem0->ns, BAD_CAST "function");

    xmlAddPrevSibling(elem0, elem0_1);

  
    xmlNodePtr elem0_1_1 = xmlNewNode(elem0_1->ns, BAD_CAST "variable");

    
    xmlNewProp(elem0_1_1, BAD_CAST "name", BAD_CAST "lambda");
  xmlAddChild(elem0_1, elem0_1_1);

  
    xmlNodePtr elem1 = getNode(doc, vars, "/jsFile", "action");
  
    xmlNodePtr elem1_1 = xmlNewNode(elem1->ns, BAD_CAST "expressionCommand");

    addFirst(elem1, elem1_1);

  
    xmlNodePtr elem1_1_1 = xmlNewNode(elem1_1->ns, BAD_CAST "funcDeclaration");

    
    xmlNewProp(elem1_1_1, BAD_CAST "name", BAD_CAST "lambda");
  xmlAddChild(elem1_1, elem1_1_1);

  
  moveLast(doc, vars, "lambda/*", "/jsFile/expressionCommand[1]/funcDeclaration");
  
  deleteNode(doc, vars, "lambda");
  
}

  

int main(int argc, char **argv) {


  const char *xpath1[] = {"lambda", NULL};
  xformTemplate template1 = {xpath1, &action1};




  xformTemplate *templates[] = {&template1, NULL};

  return readDocAndModify(argc, argv, templates);

}

  