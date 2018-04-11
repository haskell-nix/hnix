let

  s13 = ''
    start on network-interfaces

    start script
    
      rm -f /var/run/opengl-driver
      ${if true
        then "ln -sf 123 /var/run/opengl-driver"
        else if true
        then "ln -sf 456 /var/run/opengl-driver"
        else ""
      }

      rm -f /var/log/slim.log
       
    end script

    env SLIM_CFGFILE=${"abc"}
    env SLIM_THEMESDIR=${"def"}
    env FONTCONFIG_FILE=/etc/fonts/fonts.conf  				# !!! cleanup
    env XKB_BINDIR=${"foo"}/bin         				# Needed for the Xkb extension.
    env LD_LIBRARY_PATH=${"libX11"}/lib:${"libXext"}/lib:/usr/lib/          # related to xorg-sys-opengl - needed to load libglx for (AI)GLX support (for compiz)

    ${if true
      then "env XORG_DRI_DRIVER_PATH=${"nvidiaDrivers"}/X11R6/lib/modules/drivers/"
    else if true
      then "env XORG_DRI_DRIVER_PATH=${"mesa"}/lib/modules/dri"
      else ""
    } 

    exec ${"slim"}/bin/slim
  '';

in s13
