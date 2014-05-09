
void deletePet(map<string, xmlNodePtr> vars) {
  deleteNode(vars, vars["pet"]);
}

void changeNamePetToDog(map<string, xmlNodePtr> vars) {
  xmlNodePtr node = vars["pet"];
  if (node->type == XML_ELEMENT_NODE && xmlStrcmp(node->name, BAD_CAST("pet")) == 0) {
    xmlUnlinkNode(node);
    xmlFreeNode(node);
  }
}

void moveChildPetOut(map<string, xmlNodePtr> vars) {
  moveBefore(vars, vars["/child/pet"], vars["/child"]);
}

void addJob(map<string, xmlNodePtr> vars) {
  addElement(vars, "/child", "job");
  addAttribute(vars, "/child/job", "title", "supreme being");
}


int main(int argc, char **argv) {

  const char *xpath1[] = {"child", NULL};
  xformTemplate template1 = {xpath1, &addJob};
  const char *xpath2[] = {"child", "pet", NULL};
  xformTemplate template2 = {xpath2, &moveChildPetOut};
  xformTemplate *templates[] = {&template1, &template2, NULL};

  return readDocAndModify(argc, argv, templates);
  

}
