# A systemd serviceConfig attribute blob with good defaults, to be merged with
# any other services.
#
# With suggestions from:
# * https://www.redhat.com/sysadmin/systemd-secure-services
#
# TODO: Integrate suggestions from these links:
# * https://www.bountysource.com/issues/10587413-rfc-harden-ed-nixos
# * https://github.com/NixOS/nixpkgs/issues/7220
#
# Also try: systemd-analyze security <service>.service
#
# Comments below here are covered by the systemd LGPL in ../legal.
{ userService ? false }:

{
  MemoryLimit = "50%";
  MemorySwapMax = "0M";

  WorkingDirectory = "/var/empty";

  # Takes a boolean argument. If true, ensures that the service process and all
  # its children can never gain new privileges through execve() (e.g. via setuid
  # or setgid bits, or filesystem capabilities). This is the simplest and most
  # effective way to ensure that a process and its children can never elevate
  # privileges again. Defaults to false, but certain settings override this and
  # ignore the value of this setting. This is the case when SystemCallFilter=,
  # SystemCallArchitectures=, RestrictAddressFamilies=, RestrictNamespaces=,
  # PrivateDevices=, ProtectKernelTunables=, ProtectKernelModules=,
  # ProtectKernelLogs=, ProtectClock=, MemoryDenyWriteExecute=, RestrictRealtime=,
  # RestrictSUIDSGID=, DynamicUser= or LockPersonality= are specified. Note that
  # even if this setting is overridden by them, systemctl show shows the original
  # value of this setting. Also see No New Privileges Flag.
  NoNewPrivileges = true;

  # Takes a boolean argument. If set, writes to the hardware clock or system
  # clock will be denied. It is recommended to turn this on for most services
  # that do not need modify the clock.
  ProtectClock = true;

  # Takes a boolean argument. If true, explicit module loading will be denied.
  # This allows module load and unload operations to be turned off on modular
  # kernels. It is recommended to turn this on for most services that do not need
  # special file systems or extra kernel modules to work.
  ProtectKernelModules = true;
  ProtectKernelLogs = true;

  # Takes a boolean argument or the special values "full" or "strict". If true,
  # mounts the /usr and the boot loader directories (/boot and /efi) read-only for
  # processes invoked by this unit. If set to "full", the /etc directory is
  # mounted read-only, too. If set to "strict" the entire file system hierarchy is
  # mounted read-only, except for the API file system subtrees /dev, /proc and
  # /sys (protect these directories using PrivateDevices=, ProtectKernelTunables=,
  # ProtectControlGroups=). This setting ensures that any modification of the
  # vendor-supplied operating system (and optionally its configuration, and local
  # mounts) is prohibited for the service. It is recommended to enable this
  # setting for all long-running services, unless they are involved with system
  # updates or need to modify the operating system in other ways. If this option
  # is used, ReadWritePaths= may be used to exclude specific directories from
  # being made read-only. This setting is implied if DynamicUser= is set. This
  # setting cannot ensure protection in all cases. In general it has the same
  # limitations as ReadOnlyPaths=, see below. Defaults to off.
  ProtectSystem = "full";

  # Takes a boolean argument. If true, sets up a new /dev mount for the executed
  # processes and only adds API pseudo devices such as /dev/null, /dev/zero or
  # /dev/random (as well as the pseudo TTY subsystem) to it, but no physical
  # devices such as /dev/sda, system memory /dev/mem, system ports /dev/port and
  # others. This is useful to securely turn off physical device access by the
  # executed process. Defaults to false. Enabling this option will install a
  # system call filter to block low-level I/O system calls that are grouped in the
  # @raw-io set, will also remove CAP_MKNOD and CAP_SYS_RAWIO from the capability
  # bounding set for the unit (see above), and set DevicePolicy=closed (see
  # systemd.resource-control(5) for details). Note that using this setting will
  # disconnect propagation of mounts from the service to the host (propagation in
  # the opposite direction continues to work). This means that this setting may
  # not be used for services which shall be able to install mount points in the
  # main mount namespace. The new /dev will be mounted read-only and 'noexec'. The
  # latter may break old programs which try to set up executable memory by using
  # mmap(2) of /dev/zero instead of using MAP_ANON. For this setting the same
  # restrictions regarding mount propagation and privileges apply as for
  # ReadOnlyPaths= and related calls, see above. If turned on and if running in
  # user mode, or in system mode, but without the CAP_SYS_ADMIN capability (e.g.
  # setting User=), NoNewPrivileges=yes is implied.
  PrivateDevices = true;

  # Takes a boolean parameter. If set, the processes of this unit will be run in
  # their own private file system (mount) namespace with all mount propagation
  # from the processes towards the host's main file system namespace turned off.
  # This means any file system mount points established or removed by the unit's
  # processes will be private to them and not be visible to the host. However,
  # file system mount points established or removed on the host will be propagated
  # to the unit's processes. See mount_namespaces(7) for details on file system
  # namespaces. Defaults to off.
  PrivateMounts = true;

  # Takes a boolean argument. If true, sets up a new network namespace for the
  # executed processes and configures only the loopback network device "lo" inside
  # it. No other network devices will be available to the executed process. This
  # is useful to turn off network access by the executed process. Defaults to
  # false. It is possible to run two or more units within the same private network
  # namespace by using the JoinsNamespaceOf= directive, see systemd.unit(5) for
  # details.
  PrivateNetwork = true;

  # Takes a boolean argument. If true, sets up a new file system namespace for
  # the executed processes and mounts private /tmp/ and /var/tmp/ directories
  # inside it that are not shared by processes outside of the namespace. This is
  # useful to secure access to temporary files of the process, but makes sharing
  # between processes via /tmp or /var/tmp impossible. If this is enabled, all
  # temporary files created by a service in these directories will be removed
  # after the service is stopped. [...] This setting is implied if DynamicUser=
  # is set. For this setting the same restrictions regarding mount propagation
  # and privileges apply as for ReadOnlyPaths= and related calls, see above.
  PrivateTmp = true;
} //
 (if userService
  then {}
  else {
    InaccessibleDirectories = "/home";

    Group = "nobody";
    User = "nobody";

    # Takes a boolean argument or the special values "read-only" or "tmpfs". If
    # true, the directories /home, /root, and /run/user are made inaccessible and
    # empty for processes invoked by this unit. If set to "read-only", the three
    # directories are made read-only instead. If set to "tmpfs", temporary file
    # systems are mounted on the three directories in read-only mode. The value
    # "tmpfs" is useful to hide home directories not relevant to the processes
    # invoked by the unit, while still allowing necessary directories to be made
    # visible when listed in BindPaths= or BindReadOnlyPaths=.
    #
    # Setting this to "yes" is mostly equivalent to set the three directories in
    # InaccessiblePaths=. Similarly, "read-only" is mostly equivalent to
    # ReadOnlyPaths=, and "tmpfs" is mostly equivalent to TemporaryFileSystem= with
    # ":ro".
    #
    # It is recommended to enable this setting for all long-running services (in
    # particular network-facing ones), to ensure they cannot get access to private
    # user data, unless the services actually require access to the user's private
    # data. This setting is implied if DynamicUser= is set. This setting cannot ensure
    # protection in all cases. In general it has the same limitations as
    # ReadOnlyPaths=, see below.
    ProtectHome = true;
  })
