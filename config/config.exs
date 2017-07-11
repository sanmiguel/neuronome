# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

config :nerves_network, :default,
  wlan0: [
    ssid: System.get_env("NERVES_WIFI_SSID"),
    key_mgmt: :"WPA-PSK",
    psk: System.get_env("NERVES_WIFI_PSK")
  ],
  eth0: [
    ipv4_address_method: :dhcp
  ]

config :nerves_network,
  regulatory_domain: :"PL"

# Wifi config, taken from nerves-project/nerves-examples/hello_wifi
config :neuronome, :wlan0,
  ssid: System.get_env("NERVES_WIFI_SSID"),
  key_mgmt: :"WPA-PSK",
  psk: System.get_env("NERVES_WIFI_PSK")

# SSH Keys for nerves_firmware_ssh
config :nerves_firmware_ssh,
  authorized_keys: [
  ]
# Customize the firmware. Uncomment all or parts of the following
# to add files to the root filesystem or modify the firmware
# archive.

config :nerves, :firmware,
  rootfs_additions: "config/rootfs-additions"
#   fwup_conf: "config/fwup.conf"

# Import target specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
# Uncomment to use target specific configurations

# import_config "#{Mix.Project.config[:target]}.exs"
