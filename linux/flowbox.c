#include <gtk/gtk.h>
#include <math.h>
#include "flowbox.h"

#include "break.h"

/* Private class member */
#define FLOW_BOX_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE((obj), \
	FLOW_BOX_TYPE, PSquarePrivate))

typedef struct _PSquarePrivate PSquarePrivate;

struct _PSquarePrivate
{
	GList *children;
};

/* Forward declarations */
static void flow_box_get_preferred_width(GtkWidget *widget,
	int *minimal, int *natural);
static void flow_box_get_preferred_height(GtkWidget *widget,
	int *minimal, int *natural);
static void flow_box_size_allocate(GtkWidget *widget,
	GtkAllocation *allocation);
static GType flow_box_child_type(GtkContainer *container);
static void flow_box_add(GtkContainer *container, GtkWidget *widget);
static void flow_box_remove(GtkContainer *container, GtkWidget *widget);
static void flow_box_forall(GtkContainer *container, gboolean include_internals, GtkCallback callback, gpointer callback_data);

/* Define the PSquare type and inherit from GtkContainer */
G_DEFINE_TYPE(PSquare, flow_box, GTK_TYPE_CONTAINER);

/* Initialize the PSquare class */
static void
flow_box_class_init(PSquareClass *klass)
{
	/* Override GtkWidget methods */
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);
	widget_class->get_preferred_width = flow_box_get_preferred_width;
	widget_class->get_preferred_height = flow_box_get_preferred_height;
	widget_class->size_allocate = flow_box_size_allocate;

	/* Override GtkContainer methods */
	GtkContainerClass *container_class = GTK_CONTAINER_CLASS(klass);
	container_class->child_type = flow_box_child_type;
	container_class->add = flow_box_add;
	container_class->remove = flow_box_remove;
	container_class->forall = flow_box_forall;

	/* Add private indirection member */
	g_type_class_add_private(klass, sizeof(PSquarePrivate));
}

/* Initialize a new PSquare instance */
static void
flow_box_init(PSquare *square)
{
	/* This means that PSquare doesn't supply its own GdkWindow */
	gtk_widget_set_has_window(GTK_WIDGET(square), FALSE);
	/* Set redraw on allocate to FALSE if the top left corner of your widget
	 * doesn't change when it's resized; this saves time */
	/*gtk_widget_set_redraw_on_allocate(GTK_WIDGET(square), FALSE);*/

	/* Initialize private members */
	PSquarePrivate *priv = FLOW_BOX_PRIVATE(square);
	priv->children = NULL;
}

/* Return a new PSquare cast to a GtkWidget */
GtkWidget *
flow_box_new()
{
	return GTK_WIDGET(g_object_new(flow_box_get_type(), NULL));
}

/* Get the width of the container */
static void
flow_box_get_preferred_width(GtkWidget *widget, int *minimal, int *natural)
{
	g_return_if_fail(widget != NULL);
	g_return_if_fail(P_IS_SQUARE(widget));

	*minimal=0;
	*natural=0;

}

/* Get the height of the container */
static void
flow_box_get_preferred_height(GtkWidget *widget, int *minimal, int *natural)
{
	g_return_if_fail(widget != NULL);
	g_return_if_fail(P_IS_SQUARE(widget));

	*minimal=0;
	*natural=0;

}

static void
flow_box_size_allocate(GtkWidget *widget, GtkAllocation *allocation)
{

  printf("allocate\n");

  g_return_if_fail(widget != NULL || allocation != NULL);
  g_return_if_fail(P_IS_SQUARE(widget));

  gtk_widget_set_allocation(widget, allocation);
  
  unsigned border_width = gtk_container_get_border_width(GTK_CONTAINER(widget));

  int x = allocation->x + border_width, 
    y = allocation->y + border_width, 
    maxHeight = 0;

  //PSquarePrivate *priv = FLOW_BOX_PRIVATE(widget);

  GList *iter;
  int child_minimal_width, child_natural_width;
  int child_minimal_height, child_natural_height;

  GtkContainer *theContainer = GTK_CONTAINER(widget);

  GList *children = gtk_container_get_children(theContainer);

  //for(iter = priv->children; iter; iter = g_list_next(iter)) {
  for(iter = children; iter; iter = g_list_next(iter)) {

    if(!gtk_widget_get_visible(iter->data)) continue;

    gtk_widget_get_preferred_width(iter->data, &child_minimal_width, &child_natural_width);
    gtk_widget_get_preferred_height(iter->data, &child_minimal_height, &child_natural_height);

    if (
	(x + child_natural_width > 
	 allocation->x + allocation->width - border_width) 
	||
	IS_BREAK(iter->data)
	||
	GTK_IS_GRID(iter->data)
       ) {
      x = allocation->x + border_width;
      y += maxHeight;
      maxHeight = 0;
    }
    
    /* Give the child its allocation */
    GtkAllocation child_allocation;
    child_allocation.x = x;
    child_allocation.y = y;
    child_allocation.width = child_natural_width;
    child_allocation.height = child_natural_height;
    gtk_widget_size_allocate(iter->data, &child_allocation);
    
    /* Advance the x coordinate */
    x += child_allocation.width;

    if (child_allocation.height > maxHeight) 
      maxHeight = child_allocation.height;

  }
  

}

/* Return the type of children this container accepts */
static GType
flow_box_child_type(GtkContainer *container)
{
	return GTK_TYPE_WIDGET;
}

/* Add a child to the container */
static void
flow_box_add(GtkContainer *container, GtkWidget *widget)
{
	g_return_if_fail(container || P_IS_SQUARE(container));
	g_return_if_fail(widget || GTK_IS_WIDGET(widget));
	g_return_if_fail(gtk_widget_get_parent(widget) == NULL);

	PSquarePrivate *priv = FLOW_BOX_PRIVATE(container);

	/* Add the child to our list of children. 
	 * All the real work is done in gtk_widget_set_parent(). */
	priv->children = g_list_append(priv->children, widget);
	gtk_widget_set_parent(widget, GTK_WIDGET(container));

	/* Queue redraw */
	if(gtk_widget_get_visible(widget))
		gtk_widget_queue_resize(GTK_WIDGET(container));
}

/* Remove a child from the container */
static void
flow_box_remove(GtkContainer *container, GtkWidget *widget)
{
	g_return_if_fail(container || P_IS_SQUARE(container));
	g_return_if_fail(widget || GTK_IS_WIDGET(widget));

	PSquarePrivate *priv = FLOW_BOX_PRIVATE(container);

	/* Remove the child from our list of children. 
	 * Again, all the real work is done in gtk_widget_unparent(). */
	GList *link = g_list_find(priv->children, widget);
	if(link) {
		gboolean was_visible = gtk_widget_get_visible(widget);
		gtk_widget_unparent(widget);

		priv->children = g_list_delete_link(priv->children, link);

		/* Queue redraw */
		if(was_visible)
			gtk_widget_queue_resize(GTK_WIDGET(container));
	}
}

/* Call the function for all the container's children. This function
 * ignores the include_internals argument, because there are no
 * "internal" children. */
static void
flow_box_forall(GtkContainer *container, gboolean include_internals,
	GtkCallback callback, gpointer callback_data)
{
	PSquarePrivate *priv = FLOW_BOX_PRIVATE(container);
	g_list_foreach(priv->children, (GFunc)callback, callback_data);
}
