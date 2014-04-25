
#include <stdlib.h>
#include <string.h>

#include "flowbox.h"
#include "break.h"


void addH1(PSquare *container, const char *value) {
  int valueWithMarkupLength = strlen(value) + 100;
  char *valueWithMarkup = (char *) malloc(valueWithMarkupLength);

  snprintf(valueWithMarkup, valueWithMarkupLength, "<span size='32000'>%s</span>", value);

  GtkWidget *breakWidget = break_new();
  gtk_container_add (GTK_CONTAINER(container), breakWidget);
  gtk_widget_show (breakWidget);

  GtkWidget *label = gtk_label_new(value);
  gtk_label_set_markup (GTK_LABEL (label), valueWithMarkup);
  gtk_container_add (GTK_CONTAINER(container), label);
  gtk_widget_show (label);

  breakWidget = break_new();
  gtk_container_add (GTK_CONTAINER(container), breakWidget);
  gtk_widget_show (breakWidget);

  free(valueWithMarkup);


}

void addImage(PSquare *container, const char *src) {

  GtkWidget *breakWidget = break_new();
  gtk_container_add (GTK_CONTAINER(container), breakWidget);
  gtk_widget_show (breakWidget);

  GtkWidget *label = gtk_image_new_from_file(src);
  gtk_container_add (GTK_CONTAINER(container), label);
  gtk_widget_show (label);

  breakWidget = break_new();
  gtk_container_add (GTK_CONTAINER(container), breakWidget);
  gtk_widget_show (breakWidget);


}

void addButton(PSquare *container, const char *label) {

  GtkWidget *button = gtk_button_new_with_label(label);
  gtk_container_add (GTK_CONTAINER(container), button);
  gtk_widget_show (button);

}

void addLabel(PSquare *container, const char *value) {

  GtkWidget *label = gtk_label_new(value);
  gtk_container_add (GTK_CONTAINER(container), label);
  gtk_widget_show (label);

}

void addEntry(PSquare *container) {

  GtkWidget *entry = gtk_entry_new();
  gtk_container_add (GTK_CONTAINER(container), entry);
  gtk_widget_show (entry);

}

void addProgressBar(PSquare *container) {

  GtkWidget *widget = gtk_progress_bar_new();
  gtk_container_add (GTK_CONTAINER(container), widget);
  gtk_widget_show (widget);

}

void addBreak(PSquare *container) {
  GtkWidget *breakWidget = break_new();
  gtk_container_add (GTK_CONTAINER(container), breakWidget);
  gtk_widget_show (breakWidget);

}

GtkGrid *addGrid(PSquare *container) {
  GtkWidget *widget = gtk_grid_new();
  gtk_container_add (GTK_CONTAINER(container), widget);
  gtk_widget_show (widget);
  return GTK_GRID(widget);
}

void addLabelToGrid(GtkGrid *grid, const char *value, int col, int row) {
  GtkWidget *widget = gtk_label_new(value);
  gtk_grid_attach(grid, widget, row, col, 1, 1);
  gtk_widget_show (widget);
}

void addLabelToUl(GtkGrid *grid, const char *value, int row) {
  GtkWidget *dot = gtk_label_new("\xc2\xb7");
  GtkWidget *widget = gtk_label_new(value);
  gtk_grid_attach(grid, dot, 0, row, 1, 1);
  gtk_grid_attach(grid, widget, 1, row, 1, 1);
  gtk_widget_show (dot);
  gtk_widget_show (widget);
}

void addLabelToOl(GtkGrid *grid, const char *value, int row) {
  char numberString[100];
  snprintf(numberString, 100, "%d. ", row);
  GtkWidget *dot = gtk_label_new(numberString);
  GtkWidget *widget = gtk_label_new(value);
  gtk_grid_attach(grid, dot, 0, row, 1, 1);
  gtk_grid_attach(grid, widget, 1, row, 1, 1);
  gtk_widget_show (dot);
  gtk_widget_show (widget);
}

