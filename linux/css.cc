#include <gtk/gtk.h>
#include <gtk/gtkmain.h>


void applyCSSToWindow(const char *styleString) {
  GtkCssProvider *provider;
  GdkDisplay *display;
  GdkScreen *screen;
  
  provider = gtk_css_provider_new();

  display = gdk_display_get_default ();
  screen = gdk_display_get_default_screen (display);

  gtk_style_context_add_provider_for_screen (screen,
                                 GTK_STYLE_PROVIDER (provider),
                                 GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);

  gtk_css_provider_load_from_data (GTK_CSS_PROVIDER(provider), styleString,
				   /*" GtkWindow {\n"
                                   "   -GtkWidget-focus-line-width: 0;\n"
                                   "   background-color: blue;\n"              
                                   "}\n"
                                   " GtkGrid {\n"
                                   "   padding: 4px;\n"
                                   "   border: 4px solid orange;\n"  
                                   "}\n"
				   " GtkLabel {\n"
                                   //"   background-color: green;\n"  
                                   //"   border: red 10px solid;\n"  
                                   //"   border-top: orange 10px solid;\n"  
                                   //"   border-radius: 10px;\n"  
                                   "   padding: 10px;\n"  
                                   "}\n", */
				   -1, NULL);

  g_object_unref (provider);

}

void applyCSSToWidget(GtkWidget *widget) {
  GtkCssProvider *provider;
  provider = gtk_css_provider_new();

  gtk_style_context_add_provider(gtk_widget_get_style_context(widget), GTK_STYLE_PROVIDER(provider), G_MAXUINT);

  gtk_css_provider_load_from_data (GTK_CSS_PROVIDER(provider),
                                   " GtkLabel {\n"
                                   "   background-color: yellow;\n"              
                                   "   border-top: green solid 10px;\n"  
                                   "   margin: 20px;\n"
                                   "}\n", -1, NULL);


}
