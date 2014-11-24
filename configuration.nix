# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "bubbles"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless.
  security.sudo.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleKeyMap = "dk";
    defaultLocale = "en_US.utf8";
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP 
  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    emacs
    chromium
    acpi
    zsh bash
    autojump
    terminator
    wine
    python
    python27Packages.ipython

    # Skrammel 
    cmake
    gnumake
    which
    gnuplot
    unzip
  ];

  environment.interactiveShellInit = ''
    xmodmap ~/.xmodmap
    export NIX_LDFLAGS="$NIX_LDFLAGS -lgcc_s"
  '';

  services.xserver = {
    enable = true;
    layout = "dk";
    xkbOptions = "eurosign:e";
    windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
            haskellPackages.regexPcre
        ];
    };
    windowManager.default = "xmonad";
    desktopManager.default = "none";
    synaptics = {
        enable = true;
        tapButtons = false;
        twoFingerScroll = true;
        minSpeed = "2.0";
    };
  };

  fileSystems."/mnt/usb" =
    { device = "/dev/sdc1"; options = "user,noauto,noatime,flush"; };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";
  programs.zsh.enable = true;
  users.extraUsers.kris = {
    name = "kris";
    group = "users";
    extraGroups = [ "wheel" ];
    uid = 1000;
    createHome = true;
    home = "/home/kris";
    shell = "/run/current-system/sw/bin/bash";
  };

}
