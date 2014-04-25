#include <gtk/gtk.h>
#include <math.h>
#include "break.h"

/* Private class member */
#define Break_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE((obj), \
	BREAK_TYPE, BreakPrivate))

typedef struct _BreakPrivate BreakPrivate;

struct _BreakPrivate
{
};

/* Forward declarations */
static void break_get_preferred_width(GtkWidget *widget,
	int *minimal, int *natural);
static void break_get_preferred_height(GtkWidget *widget,
	int *minimal, int *natural);
//static void break_size_allocate(GtkWidget *widget,
//	GtkAllocation *allocation);

G_DEFINE_TYPE(Break, break, GTK_TYPE_WIDGET);

static void
break_class_init(BreakClass *klass)
{
	/* Override GtkWidget methods */
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);
	widget_class->get_preferred_width = break_get_preferred_width;
	widget_class->get_preferred_height = break_get_preferred_height;
	//widget_class->size_allocate = break_size_allocate;

	/* Add private indirection member */
	g_type_class_add_private(klass, sizeof(BreakPrivate));
}

static void
break_init(Break *breakWidget)
{
	gtk_widget_set_has_window(GTK_WIDGET(breakWidget), FALSE);

	/* Initialize private members */
	BreakPrivate *priv = Break_PRIVATE(breakWidget);
}

GtkWidget *
break_new()
{
	return GTK_WIDGET(g_object_new(break_get_type(), NULL));
}

/* Get the width of the container */
static void
break_get_preferred_width(GtkWidget *widget, int *minimal, int *natural)
{
	g_return_if_fail(widget != NULL);
	g_return_if_fail(IS_BREAK(widget));

	*minimal=0;
	*natural=0;

}

/* Get the height of the container */
static void
break_get_preferred_height(GtkWidget *widget, int *minimal, int *natural)
{
	g_return_if_fail(widget != NULL);
	g_return_if_fail(IS_BREAK(widget));

	*minimal=0;
	*natural=0;

}
