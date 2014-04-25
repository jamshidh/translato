#ifndef FLOW_BOX_H
#define FLOW_BOX_H

#include <glib-object.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define FLOW_BOX_TYPE            (flow_box_get_type())
#define FLOW_BOX(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), FLOW_BOX_TYPE, PSquare))
#define FLOW_BOX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), FLOW_BOX_TYPE, PSqaureClass))
#define P_IS_SQUARE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), FLOW_BOX_TYPE))
#define P_IS_SQUARE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), FLOW_BOX_TYPE))

typedef struct _PSquare       PSquare;
typedef struct _PSquareClass  PSquareClass;

struct _PSquare
{
	GtkContainer parent_instance;
};

struct _PSquareClass
{
	GtkContainerClass parent_class;
};

GType flow_box_get_type(void) G_GNUC_CONST;
GtkWidget *flow_box_new(void);

G_END_DECLS

#endif /* FLOW_BOX_H */
