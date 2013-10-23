

module WidgetSizes where

import Foreign
import Graphics.UI.Gtk

#include <gtk/gtk.h>

instance Storable TextView where
  sizeOf _ = {#sizeof GtkTextView #}
--  sizeOf _ = 352
  alignment _ = 8