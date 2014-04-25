#ifndef BREAK_H
#define BREAK_H

#include <glib-object.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define BREAK_TYPE                  (break_get_type())
#define BREAK(obj)                  (G_TYPE_CHECK_INSTANCE_CAST((obj), BREAK_TYPE, BR))
#define BREAK_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST((klass), BREAK_TYPE, BRClass))
#define IS_BREAK(obj)               (G_TYPE_CHECK_INSTANCE_TYPE((obj), BREAK_TYPE))
#define IS_BREAK_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE((klass), BREAK_TYPE))

typedef struct _Break       Break;
typedef struct _BreakClass  BreakClass;

struct _Break
{
	GtkContainer parent_instance;
};

struct _BreakClass
{
	GtkContainerClass parent_class;
};

GType break_get_type(void) G_GNUC_CONST;
GtkWidget *break_new(void);

G_END_DECLS

#endif /* BREAK_H */
