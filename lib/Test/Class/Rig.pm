package Test::Class::Rig;
use lib qw( C:\johns\Dropbox\Code_Base\CPAN\Test-Class-Rig\lib );
#use lib qw( C:\Users\John Scoles\Dropbox\Code_Base\CPAN\Test-Class-Rig\lib);
BEGIN {
  $Test::Class::Rig::VERSION = "0.01";
};

use Exporter;
BEGIN {
  @ISA = qw(Exporter );
  @EXPORT    = ();
  @EXPORT_OK = qw(%Test::Class::Rig );
}
use 5.010;
use strict;
use warnings;
use parent qw(Orignal );
use Carp;
use DBI;
use Data::Dumper;
my %is_flag_attribute = map {$_ =>1 } qw(
   PrintError
   PrintWarn
   RaiseError
   Warn
   HandleError
);
my %is_valid_attribute = map {$_ =>1 } (keys %is_flag_attribute, qw(
   Version
   errstr
   Debug
   Agent
   AgentClass
));
my %valid_agents = (tra=>'agent',
                    tref=>'email_file_system',
                    trec=>'email_client' );
%Test::Class::Rig::installed_trd = ();  # maps driver names to installed driver handles
%Test::Class::Rig::installed_tra = ();  # maps agent names to installed agent handles
%Test::Class::Rig::installed_tref = (); # maps email file_system name to installed email file_system handles
%Test::Class::Rig::installed_trec = (); # maps email client to installed email client handles
# check for weaken support, used by ChildHandles
my $HAS_WEAKEN = eval {
    require Scalar::Util;
    # this will croak() if this Scalar::Util doesn't have a working weaken().
    &Scalar::Util::weaken( my $test = [] );
    1;
};
# sub before : Test(setup)    { diag("running before test") };
# sub after  : Test(teardown) { diag("running after test") };
sub installed_drivers { %Test::Class::Rig::installed_trd }
sub installed_agents { %Test::Class::Rig::installed_tra }
sub installed_email_file_systems { %Test::Class::Rig::installed_tref }
sub installed_email_clients { %Test::Class::Rig::installed_trec }
sub visit_handles {
    my ($class, $code, $outer_info) = @_;
    $outer_info = {} if not defined $outer_info;
    my %drh = Test::Class::Rig->installed_drivers;
    for my $h (values %drh) {
       my $child_info = $code->($h, $outer_info)
          or next;
      $h->visit_child_handles($code, $child_info);
    }
    return $outer_info;
}
sub visit_child_handles {
    my ($h, $code, $info) = @_;
    $info = {} if not defined $info;
    for my $ch (@{ $h->{ChildHandles} || []}) {
      next unless $ch;
      my $child_info = $code->($ch, $info)
        or next;
      $ch->visit_child_handles($code, $child_info);
    }
    return $info;
}
sub installed_versions {
    my ($class, $quiet) = @_;
    my %error;
    my %version = ( 'Test::Class::Rig' => $Test::Class::Rig::VERSION);
    for my $driver ($class->available_drivers($quiet)) {
        my $trd = eval {
           local $SIG{__WARN__} = sub {};
           $class->install_driver($driver);
        };
        ($error{"Test::Class::TRD::$driver"}=$@),next if $@;
        no strict 'refs';
        my $vers = ${"Test::Class::TRD::$driver" . '::VERSION'};
        $version{"Test::Class::TRD::$driver"} = $vers || '?';
    }
    if (wantarray) {
        return map { m/^Test::Class::TRD::(\w+)/ ? ($1) : () } sort keys %version;
    }
    if (!defined wantarray) {	# void context
        require Config;		# add more detail
        $version{OS}   = "$^O\t($Config::Config{osvers})";
        $version{Perl} = "$]\t($Config::Config{archname})";
        $version{$_}   = (($error{$_} =~ s/ \(\@INC.*//s),$error{$_})
        for keys %error;
        printf "  %-16s: %s\n",$_,$version{$_}
        for reverse sort keys %version;
    }
    return \%version;
}
{   # used to catch ExtractRig->{Attrib} mistake
   sub Test::Class::Rig::Rig_tie::TIEHASH { bless {} }
   sub Test::Class::Rig::Rig_tie::STORE   { Carp::carp("Test::Class::Rig->{$_[1]} is invalid syntax (you probably want \$h->{$_[1]})");}
   *Test::Class::Rig::Rig_tie::FETCH = \&Test::Class::Rig::Rig_tie::STORE;
}
tie %Test::Class::Rig::rig => 'Test::Class::Rig::Rig_tie';
sub Test::Class::Rig::common::TIEHASH {  bless $_[1] => $_[0];};
*Test::Class::Rig::trd::TIEHASH = \&Test::Class::Rig::common::TIEHASH;

sub connect {
   my $self = shift;
   my ($attr) = @_;
   
   Carp::croak("Fatal Error Test::Class::Rig::rig->connect()! Attributes must be a HASH Referance or empty!")
      unless(ref($attr) eq "HASH" and $attr);
      
   my $rig = $self->{rig};
   my $dbh = "";
   
   my $dbd_name    = $rig->dbd_name()   || $attr->{dbd_name};
   my $db_name     = $rig->db_name()    || $attr->{db_name};
   my $db_user_id  = $rig->db_user_id() || $attr->{db_user_id};
   my $db_password = $rig->db_password()|| $attr->{db_password};
   my $db_schema   = $rig->db_schema()  || $attr->{db_schema};
   
   
   my $db_attributes       = $rig->db_attributes()      || $attr->{db_attributes};
   my $db_date_time_format = $rig->db_date_time_format()|| $attr->{db_date_time_format};
   
      
   $dbh = DBI->connect("dbi:".$dbd_name.":".$db_name,$db_user_id,$db_password, $db_attributes );
   
   if ($db_schema){
      $dbh->do("alter session set current_schema  = $db_schema");
   }
   if ($db_date_time_format){
      $dbh->do("alter session set NLS_DATE_FORMAT = '$db_date_time_format'");
   }
   
   return($dbh);
}


sub _handle_error {
   my $self=shift;
   my ($err,$method_name) = @_;
   my($pe,$pw,$re,$he) = @{$self}{qw(PrintError PrintWarn RaiseError HandleError)};
   my $msg;
   if ($err && ($pe || $re || $he) # error
      or (!$err && length($err) && $pw)# warning
   ) {
      my $errstr = $self->{errstr} || $err || '';
      $msg = sprintf "%s %s %s: %s", $self, $method_name,
              ($err eq "0") ? "Warning" : "Failed", $errstr;
      if ($err eq "0") { # is 'warning' (not info)
         carp $msg if $pw;
      }
      else {
         my $do_croak = 1;
         if (my $subsub = $self->{'HandleError'}) {
            $do_croak = 0 if &$subsub($msg,$self);
          }
          if ($do_croak) {
             printf $self "    $method_name has failed ($self->{PrintError},$self->{RaiseError})\n"
                if ($self->TRACE() & 0xF) >= 4;
             carp  $msg if $pe;
             die $msg if $self->{RaiseError};
          }
      }
   }
   $self->{errstr}="";
}
sub available_drivers {
    my ($quiet) = @_;
    my @drivers = _available($quiet,'TRD');
    return wantarray ? sort @drivers : @drivers;
}
sub available_agents {
    my ($quiet) = @_;
    my @agents = _available($quiet,'TRA');
    return wantarray ? sort @agents : @agents;
}
sub available_email_file_systems {
    my ($quiet) = @_;
    my @email_fs = _available($quiet,'TREF');
    return wantarray ? sort @email_fs : @email_fs;
}
sub available_email_clients {
    my ($quiet) = @_;
    my @email_clients = _available($quiet,'TREC');
    return wantarray ? sort @email_clients : @email_clients;
}
sub _available {
   my ($quiet,$type) = @_;
   my (@drivers, $d, $f);
   local(*Test::Class::Rig::DIR, $@);
   my(%seen_dir, %seen_trd);
   my $haveFileSpec = eval { require File::Spec };
   foreach my $d (@INC){
       chomp($d); # Perl 5 beta 3 bug in #!./perl -Ilib from Test::Harness
       my $trd_dir = ($haveFileSpec ? File::Spec->catdir($d, "Test/Class/$type") : "$d/Test/Class/$type");
       next
         unless -d $trd_dir;
       next
         if $seen_dir{$d};
       $seen_dir{$d} = 1;
       # XXX we have a problem here with case insensitive file systems
       # XXX since we can't tell what case must be used when loading.
       opendir(Test::Class::Rig::DIR, $trd_dir) || Carp::carp "opendir $trd_dir: $!\n";
       foreach my $f (readdir(Test::Class::Rig::DIR)){
          next
            unless $f =~ s/\.pm$//;
          next
            if $f eq 'NullP';
          if ($seen_trd{$f}){
             Carp::carp "Test::Class::Rig::$f in $d is hidden by Test::Class::Rig::$f in $seen_trd{$f}\n"
               unless $quiet;
          }
          else {
             push(@drivers, $f);
          }
          $seen_trd{$f} = $d;
       }
       closedir(Test::Class::Rig::DIR);
    }
    # "return sort @drivers" will not DWIM in scalar context.
    return @drivers;
}
sub load {
   my $class = shift;
   die ("Can't locate object method \"load\" via package \"".ref($class)."\"")
     if($class ne 'Test::Class::Rig');
   my ($driver,$rig_attr,$attr) = @_; #save for later
   $attr={}
     unless($attr);
   my $trdh = $Test::Class::Rig::installed_trd{$driver} || $class->install_driver($driver,$attr)
      or die "panic: ".ref($class)."->install_driver($driver) failed";
   if (exists($rig_attr->{agent})){
     my $trah = $Test::Class::Rig::installed_tra{$rig_attr->{agent}} || $class->install_agent($rig_attr->{agent},$attr)
       or die "panic: ".ref($class)."->install_agent($rig_attr->{agent},$attr) failed";
   }
   if (exists($rig_attr->{email_file_system})){
#     warn("Ping I am here 1");
     my $trah = $Test::Class::Rig::installed_tref{$rig_attr->{email_file_system}} || $class->install_email_file_system($rig_attr->{email_file_system},$attr)
       or die "panic: ".ref($class)."->install_email_file_system($rig_attr->{email_file_system},$attr) failed";
   }
#        warn("Ping I am here load 3\n");
   if (exists($rig_attr->{email_client})){
     my $trah = $Test::Class::Rig::installed_trec{$rig_attr->{email_client}} || $class->install_email_client($rig_attr->{email_client},$attr)
       or die "panic: ".ref($class)."->install_email_clients($rig_attr->{email_client},$attr) failed";
   }
   my $rig = $trdh->load_rig($rig_attr,$attr); # called to Test::Class::Rig::<drivername>::trd::load_rig()
#        warn("Ping I am here load 4\n");
   return $rig;
}
# this is my overridden set_attributes for orginal?
  sub _set_attributes {
    my $self = shift;
    my ($config_hash) = @_;
    foreach my $key (keys(%{$config_hash})){
      if (UNIVERSAL::can($self,$key)){
         $self->$key($config_hash->{$key});
         my $value = $self->$key()||"";
         $self->_debug('_set_attributes'," $key set to '$value'")
               if ($self->TRACE() >= 4)
      }
      else {
           $self->{errstr} =  "No attribute '$key' found ";
           exit;
           $self->_handle_error(0,'_set_attributes');
      }
    }
  }
  sub _debug {
    my $self = shift;
    my ($sub,$mesg) = @_;
    my $class = $self->{ImplementorClass};
    warn("Test::Class::Rig::".ref($class)."->$sub(), ".$mesg."\n");
  }
  sub _yaml_to_hash {
    use YAML;
    my $self=shift;
    my ($file,$dir)=@_;
    $dir.="/"
      unless(substr($dir,-1,1) eq "/");
    $self->_debug('_yaml_to_hash',"Reading YML file ".$dir.$file)
       if ($self->TRACE() >= 4);
    my $hash_ref;
    eval{
      $hash_ref = YAML::LoadFile($dir.$file);
    };
    if ($@){
      my $err = $@;
      Carp::carp(ref($self)."->_yaml_to_hash '$dir$file' could not parse with this error=".$err);
    }
    return $hash_ref;
  }
  sub _setup_email_file_system {
    my ($class, $fs_class) = @_;
    my $type;
    foreach my $type (qw(tref)){
      my $class=$fs_class."::$type";
      no strict 'refs';
      push(@{"${class}::ISA"},"Test::Class::Rig::$type")
        unless UNIVERSAL::isa($class, "Test::Class::Rig::$type");
    }
  }
  sub _setup_email_client {
    my ($class, $fs_class) = @_;
    my $type;
    foreach my $type (qw(trec)){
      my $class=$fs_class."::$type";
      no strict 'refs';
      push(@{"${class}::ISA"},"Test::Class::Rig::$type")
        unless UNIVERSAL::isa($class, "Test::Class::Rig::$type");
    }
  }
  sub _setup_agent {
    my ($class, $agent_class) = @_;
    my $type;
    foreach my $type (qw(tra)){
      my $class=$agent_class."::$type";
      no strict 'refs';
      push(@{"${class}::ISA"},"Test::Class::Rig::$type")
        unless UNIVERSAL::isa($class, "Test::Class::Rig::$type");
    }
  }
  sub _setup_driver {
    my ($class, $driver_class) = @_;
    my $type;
    foreach my $type (qw(tra trd rig case default requirement tests  )){
      my $class=$driver_class."::$type";
      no strict 'refs';
      push(@{"${class}::ISA"},"Test::Class::Rig::$type")
        unless UNIVERSAL::isa($class, "Test::Class::Rig::$type");
    }
  }
  sub _setup_object {
    my ($class,$attr) = @_;
    foreach (qw( RaiseError PrintError PrintWarn TRACE)) {
      if (exists($attr->{$_})){
         $class->$_($attr->{$_})
      }
      else {
        $class->$_(0);
      }
    }
  }
  sub _setup_handle {
    my($h, $imp_class, $parent) = @_;
    my $h_inner = tied(%$h) || $h;
    $h_inner->{"ImplementorClass"} = $imp_class;
    $h_inner->{"Kids"} = $h_inner->{"ActiveKids"} = 0;
    if ($parent) {
      foreach (qw( RaiseError PrintError PrintWarn TRACE)) {
        $h_inner->$_($parent->$_())
          if ($parent->$_() && !$h_inner->$_());
      }
      if (ref($parent) =~ /::trd$/) {
        $h_inner->{Driver} = $parent;
      }
      $h_inner->{trd_pp_parent} = $parent;
      # add to the parent's ChildHandles
      if ($HAS_WEAKEN) {
        my $handles = $parent->{ChildHandles} ||= [];
        push(@{$handles}, $h);
        Scalar::Util::weaken($handles->[-1]);
        # purge destroyed handles occasionally
        if (@$handles % 120 == 0) {
            @$handles = grep { defined } @$handles;
            Scalar::Util::weaken($_) for @$handles; # re-weaken after grep
        }
      }
    }
    else {	# setting up a driver handle
      $h_inner->{Warn}		= 1;
      $h_inner->{PrintWarn}	= $^W;
      $h_inner->{TraceLevel}	= 0;
      $h_inner->{ChildHandles}||= [] if $HAS_WEAKEN;
      $h_inner->{Type}	      ||= 'trd';
      $h_inner->TRACE(0);
    }
    $h_inner->{"trd_call_depth"} = 0;
    $h_inner->{ErrCount} = 0;
    $h_inner->{Active} = 1;
  }
  sub _new_handle {
    my ($class, $parent, $attr, $use_class, $imp_class) = @_;
    $attr->{ImplementorClass} = $imp_class
      or Carp::croak("_new_handle($class): 'ImplementorClass' attribute not given");
    # This is how we create a Test::Class::Rig style Object:
    # %outer gets tied to %$attr (which becomes the 'inner' handle)
    my (%outer, $i, $h);
    if ($use_class){
      $i = tie    %outer, $class, $attr;  # ref to inner hash (for driver)
      $h = bless \%outer, $class;         # ref to outer hash (for application)
    }
    else {
      $i = tie    %outer, $imp_class, $attr;  # ref to inner hash (for driver)
      $h = bless \%outer, $imp_class;         # ref to outer hash (for application)
    }
    # The above tie and bless may migrate down into _setup_handle()...
    # Now add magic so Test::Class::Rig method dispatch works
    Test::Class::Rig::_setup_handle($h, $imp_class, $parent);
    return $h unless wantarray;
    return ($h, $i);
  }
  sub _new_rig {	# called by Test::Class::Rig::<drivername>::trd::load()
    my $self = shift;
    my ($rig_attr, $attr) = @_;
#    warn("_new_rig 1\n");
    my $imp_class = $self->{ImplementorClass}
      or Carp::croak("Test::Class::Rig::_new_rig:: $self has no ImplementorClass");
    substr($imp_class,-5,5) = '::rig';
    my $app_class = ref $self;
    #my ($rig, $i) = _new_handle($app_class, $self, $attr, 0, $imp_class);
    #my ($rig, $i) = _new_handle($app_class, $self, $attr, 0, $imp_class);
    my $rig = $imp_class->new($attr);
    $rig->{ImplementorClass}=$app_class;
    $self->_debug('_new_rig',"New Rig Created")
      if ($self->TRACE() >= 4);
    $rig->_setup_object($attr);
    $rig->set_default_dirs( $self->driver_dir());
    $rig->load_config();
    use Data::Dumper;
#    warn(Dumper($attr));
    foreach my $agent (values(%valid_agents)){
#      warn("_new_rig 2 agent=$agent\n");
      next
        unless($rig_attr->{$agent});
      my $trah      = undef;
      my $rig_agent = undef;
      given ($agent) {
        when ('agent') {
          $trah = $Test::Class::Rig::installed_tra{$rig_attr->{$agent}} || $rig->install_agent($rig_attr->{$agent},$attr)
            or die "panic: ".ref($self)."->install_agent($agent,$attr) failed";
          $rig_agent = $trah->load_agent($self);   #Call to Test::Class::TRA::<agentname>::tra to get a new agent
        };
        when ('email_file_system') {
          $trah = $Test::Class::Rig::installed_tref{$rig_attr->{$agent}} || $rig->install_email_file_system($rig_attr->{$agent},$attr)
            or die "panic: ".ref($self)."->install_email_file_system($agent,$attr) failed";
          $rig_agent = $trah->load_agent($self);   #Call to Test::Class::TREF::<agentname>::tref to get a new agent
        };
        when ('email_client') {
          $trah = $Test::Class::Rig::installed_trec{$rig_attr->{$agent}} || $rig->install_email_client($rig_attr->{$agent},$attr)
            or die "panic: ".ref($self)."->install_email_client($agent,$attr) failed";
          $rig_agent = $trah->load_agent($self);   #Call to Test::Class::TREC::<agentname>::trec to get a new agent
        };
      }
      $rig->{$agent}=$rig_agent;
    }
    $rig->load_defaults();
    $rig->import_cases();
    $rig->import_requirments();
    return   ($rig);
  }
  sub _new_sub_class {
    my $self = shift;
    my ($imp_class,$new_class) =@_;
    $new_class= $imp_class."::".$new_class;
    {
       no strict 'refs';
       eval qq{package        # hide from PAUSE
             $new_class;    # load the doc local class
              };
       push(@{"${new_class}::ISA"},$imp_class);
    }
    $self->_debug('_new_sub_class',"New Sub Class $imp_class Created")
      if ($self->TRACE() >= 5);
    return $new_class;
  }
  sub _new_trdrh { # called by Test::Class::Rig::trd::<drivername>::driver()
    my ($class, $initial_attr) = @_;
    # Provide default storage for State,Err and Errstr.
    # Note that these are shared by all child handles by default! XXX
    my ($h_state_store, $h_err_store, $h_errstr_store) = (undef, 0, '');
    $initial_attr->{TRACE} = 0
      if (!exists($initial_attr->{TRACE}));
    my $attr = { #these attributes get copied down to child handles by default
                'State'          => \$h_state_store,  # Holder for Test::Class::Rig::state
                'Err'            => \$h_err_store,    # Holder for Test::Class::Rig::err
                'Errstr'         => \$h_errstr_store, # Holder for Test::Class::Rig::errstr
                'TraceLevel'     => 0,
                FetchHashKeyName => 'NAME',
                %$initial_attr,
    };
    my ($h, $i) = _new_handle('Test::Class::Rig::trd', '', $attr,0 , $class);
    return $h
      unless wantarray;
    rerurn ($h, $i);
  }
  sub _new_trecrh { # called by Test::Class::Rig::TREC::<agentname>
    my ($class,$agent_class, $initial_attr) = @_;
    # Provide default storage for State,Err and Errstr and
    # set up the base class with the agent
    # Note that these are shared by all child handles by default! XXX
    my ($h_state_store, $h_err_store, $h_errstr_store) = (undef, 0, '');
    $initial_attr->{TRACE} = 0
      if (!exists($initial_attr->{TRACE}));
    my $attr = { #these attributes get copied down to child handles by default
                'State'          => \$h_state_store,  # Holder for Test::Class::Rig::state
                'Err'            => \$h_err_store,    # Holder for Test::Class::Rig::err
                'Errstr'         => \$h_errstr_store, # Holder for Test::Class::Rig::errstr
                'TraceLevel'     => 0,
                FetchHashKeyName => 'NAME',
                %$initial_attr,
    };
    my ($h, $i) = _new_handle('Test::Class::Rig::trec', '', $attr,0 , $class);
    $h->{AgentClass}=$agent_class;
    return $h
      unless wantarray;
    rerurn ($h, $i);
  }
  sub _new_trarh { # called by Test::Class::Rig::TRA::<agentname>::agent()
    my ($class,$agent_class, $initial_attr) = @_;
    # Provide default storage for State,Err and Errstr and
    # set up the base class with the agent
    # Note that these are shared by all child handles by default! XXX
    my ($h_state_store, $h_err_store, $h_errstr_store) = (undef, 0, '');
    $initial_attr->{TRACE} = 0
      if (!exists($initial_attr->{TRACE}));
    my $attr = { #these attributes get copied down to child handles by default
                'State'          => \$h_state_store,  # Holder for Test::Class::Rig::state
                'Err'            => \$h_err_store,    # Holder for Test::Class::Rig::err
                'Errstr'         => \$h_errstr_store, # Holder for Test::Class::Rig::errstr
                'TraceLevel'     => 0,
                FetchHashKeyName => 'NAME',
                %$initial_attr,
    };
    my ($h, $i) = _new_handle('Test::Class::Rig::tra', '', $attr,0 , $class);
    $h->{AgentClass}=$agent_class;
    return $h
      unless wantarray;
    rerurn ($h, $i);
  }
  sub _handles {
    my $h = shift;
    my $h_inner = tied %$h;
    if ($h_inner) {# this is okay
      return $h
        unless wantarray;
      return ($h, $h_inner);
    }
    # XXX this isn't okay... we have an inner handle but
    # currently have no way to get at its outer handle,
    # so we just warn and return the inner one for both...
    Carp::carp("Can't return outer handle from inner handle using Test::Class::Rig");
    return $h
      unless wantarray;
    return ($h,$h);
}
  sub install_driver {# croaks on failure.
                      # Public but not part of the POD as it is public to the other drivers
    my $class = shift;
    my($driver, $attr) = @_;
    my $trdh;
    $driver ||= $ENV{TR_DRIVER} || '';
    Carp::croak("usage: ".ref($class)."->install_driver(\$driver [, \%attr])")
      unless ($driver and @_<=3);
    # already installed
    return $trdh if $trdh = $Test::Class::Rig::installed_trd{$driver};
    # --- load the code
    my $driver_class = "Test::Class::TRD::$driver";
    eval qq{package                   # hide from PAUSE
             Test::Class::Rig::_firesafe;    # ensures that the TRD is present in the path
             require $driver_class;    # load the driver
    };
    if ($@) {
      my $err = $@;
      my $advice = "";
      if ($err =~ /Can't locate.*?TRD\/$driver\.pm in \@INC/) {
        my @drv = $class->available_drivers(1);
        $advice = "Perhaps the TRD::$driver perl module hasn't been fully installed,\n"
        ."or perhaps the capitalisation of '$driver' isn't right.\n"
        ."Available drivers: ".join(", ", @drv).".";
      }
      elsif ($err =~ /Can't load .*? for module TRD::/) {
        $advice = "Perhaps a required shared library or dll isn't installed where expected";
      }
      elsif ($err =~ /Can't locate .*? in \@INC/) {
        $advice = "Perhaps a module that TRD::$driver requires hasn't been fully installed";
      }
      Carp::croak("install_driver($driver) failed: $err$advice\n");
    }
    my $driver_dir;
    my $trd_ver;
    {#need this so I can use no strict below
      no strict 'refs';
      (my $driver_file = $driver_class) =~ s/::/\//g;
      $trd_ver    = ${"$driver_class\::VERSION"} || "undef";
      $driver_dir = substr($INC{qq($driver_file.pm)},0,(length($INC{qq($driver_file.pm)})-3));
    }
    # --- do some behind-the-scenes checks and setups on the driver
    $class->_setup_driver($driver_class);
    # --- run the driver function
    #warn("install_driver $driver_class\n");
    $trdh = eval { $driver_class->driver($attr || {}) }; #call out to  Test::Class::TRD::dirvername
    $trdh->_setup_object($attr);
    $trdh->_debug('install_driver',"install_driver: $driver_class version $trd_ver loaded from  loaded from $driver_dir.pm!")
      if ($trdh->TRACE() >= 1);
    unless ($trdh && ref $trdh && !$@) {
      my $advice = "";
      $@ ||= "$driver_class->driver didn't return a handle";
      # catch people on case in-sensitive systems using the wrong case
      $advice = "\nPerhaps the capitalisation of Test::Class::TRD '$driver' isn't right."
        if $@ =~ /locate object method/;
      Carp::croak("$driver_class initialisation failed: $@$advice");
    }
    #warn("driver_dir $trdh string = '".$driver_dir."'\n");
    $trdh->driver_dir($driver_dir);
    #warn("\n\n\n driver_dir from set-getter =".$trdh->driver_dir()."\n");
    $Test::Class::Rig::installed_trd{$driver} = $trdh;
    return $trdh;
  }
  *driver = \&install_driver;
  sub install_agent {# croaks on failure
    my $class = shift;
    my($agent, $attr) = @_;
    my $trah;
    $agent ||= $ENV{TRD_AGENT} || '';
    Carp::croak("usage: $class->install_agent(\$agent [, \%attr])")
      unless ($agent and @_<=3);
    # already installed
    return $trah
      if $trah = $Test::Class::Rig::installed_tra{$agent};
    # --- load the code
    my $agent_class = "Test::Class::TRA::$agent";
    eval qq{package                   # hide from PAUSE
            Test::Class::Rig::_firesafe;    # ensures that the TRA is present in the path
             require $agent_class;    # load the driver
          };
    if ($@) {
      my $err = $@;
      my $advice = "";
      if ($err =~ /Can't locate.*?TRA\/$agent\.pm in \@INC/) {
        my @ags = $class->available_agents(1);
        $advice = "Perhaps the TRA::$agent perl module hasn't been fully installed,\n"
        ."or perhaps the capitalisation of '$agent' isn't right.\n"
        ."Available Agents: ".join(", ", @ags).".";
      }
      elsif ($err =~ /Can't load .*? for module TRA::/) {
        $advice = "Perhaps a required shared library or dll isn't installed where expected";
      }
      elsif ($err =~ /Can't locate .*? in \@INC/) {
        $advice = "Perhaps a module that TRA::$agent requires hasn't been fully installed";
      }
      Carp::croak("install_agent($agent) failed: $err$advice\n");
    }
    my $agent_dir;
    my $tra_ver;
    {#need this so I can use no strict below
       no strict 'refs';
       (my $agent_file = $agent_class) =~ s/::/\//g;
       $tra_ver = ${"$agent_class\::VERSION"} || "undef";
       $agent_dir=substr($INC{qq($agent_file.pm)},0,(length($INC{qq($agent_file.pm)})-3));
    }
    # --- do some behind-the-scenes checks and setups on the driver
    $class->_setup_agent($agent_class);
    $trah = eval { $agent_class->agent($attr || {}) }; #call to called by Test::Class::Rig::TRA::<agentname>
    $trah->_debug('install_agent',"install_agent: $agent_class version $tra_ver loaded from  loaded from $agent_dir.pm!")
      if ($trah->TRACE() >= 1);
    unless ($trah && ref $trah && !$@) {
      my $advice = "";
      $@ ||= "$agent_class->agent didn't return a handle";
      # catch people on case in-sensitive systems using the wrong case
      $advice = "\nPerhaps the capitalisation of Test::Class::TRA '$agent' isn't right."
        if $@ =~ /locate object method/;
      Carp::croak("$agent_class initialisation failed: $@$advice");
    }
    #$trah->dir($agent_dir);
    $Test::Class::Rig::installed_tra{$agent} = $trah;
    return $trah;
  }
  *agent = \&install_agent;
  sub install_email_file_system {# croaks on failure
    my $class = shift;
    my($file_system, $attr) = @_;
    my $trefh;
    $file_system ||= $ENV{TRD_EMAIL_FILE_SYSTEM} || '';
    Carp::croak("usage: $class->install_email_account_fs(\$file_system [, \%attr])")
      unless ($file_system and @_<=3);
    # already installed
    return $trefh
      if $trefh = $Test::Class::Rig::installed_tref{$file_system};
    # --- load the code
    my $fs_class = "Test::Class::TREF::$file_system";
    eval qq{package                   # hide from PAUSE
            Test::Class::Rig::_firesafe;    # ensures that the TREF is present in the path
             require $fs_class;    # load the driver
          };
    if ($@) {
      my $err = $@;
      my $advice = "";
      if ($err =~ /Can't locate.*?TREF\/$file_system\.pm in \@INC/) {
        my @ags = $class->available_email_file_systems(1);
        $advice = "Perhaps the TREF::$file_system perl module hasn't been fully installed,\n"
        ."or perhaps the capitalisation of '$file_system' isn't right.\n"
        ."Available Email File Systems: ".join(", ", @ags).".";
      }
      elsif ($err =~ /Can't load .*? for module TREF::/) {
        $advice = "Perhaps a required shared library or dll isn't installed where expected";
      }
      elsif ($err =~ /Can't locate .*? in \@INC/) {
        $advice = "Perhaps a module that TRFA::$file_system requires hasn't been fully installed";
      }
      Carp::croak("install_agent($file_system) failed: $err$advice\n");
    }
    my $fs_dir;
    my $tref_ver;
    {#need this so I can use no strict below
       no strict 'refs';
       (my $fs_file = $fs_class) =~ s/::/\//g;
       $tref_ver = ${"$fs_class\::VERSION"} || "undef";
       $fs_dir   = substr($INC{qq($fs_file.pm)},0,(length($INC{qq($fs_file.pm)})-3));
    }
    # --- do some behind-the-scenes checks and setups on the driver
    $class->_setup_email_file_system($fs_class);
    $trefh = eval { $fs_class->email_file_system($attr || {}) }; #call to called by Test::Class::Rig::TREF::<fs_name>
    $trefh->_debug('install_email_file_system',"install_email_file_system: $fs_class version $tref_ver loaded from  loaded from $fs_dir.pm!")
      if ($trefh->TRACE() >= 1);
    unless ($trefh && ref $trefh && !$@) {
      my $advice = "";
      $@ ||= "$fs_class->Email Files Sytems didn't return a handle";
      # catch people on case in-sensitive systems using the wrong case
      $advice = "\nPerhaps the capitalisation of Test::Class::TREF '$file_system' isn't right."
        if $@ =~ /locate object method/;
      Carp::croak("$fs_class initialisation failed: $@$advice");
    }
    $Test::Class::Rig::installed_tref{$file_system} = $trefh;
    return $trefh;
  }
  *email_file_system = \&install_email_file_system;
 sub install_email_client {# croaks on failure
    my $class = shift;
    my($email_client, $attr) = @_;
#    warn("install_email_client 1\n");
    my $trech;
    $email_client ||= $ENV{TRD_EMAIL_CLIENT} || '';
    Carp::croak("usage: $class->install_email_client(\$email_client [, \%attr])")
      unless ($email_client and @_<=3);
    # already installed
    return $trech
      if $trech = $Test::Class::Rig::installed_trec{$email_client};
    # --- load the code
    my $ec_class = "Test::Class::TREC::$email_client";
    eval qq{package                   # hide from PAUSE
            Test::Class::Rig::_firesafe;    # ensures that the TREC is present in the path
             require $ec_class;    # load the driver
          };
    if ($@) {
      my $err = $@;
      my $advice = "";
      if ($err =~ /Can't locate.*?TREC\/$email_client\.pm in \@INC/) {
        my @ags = $class->available_email_clients(1);
        $advice = "Perhaps the TREC::$email_client perl module hasn't been fully installed,\n"
        ."or perhaps the capitalisation of '$email_client' isn't right.\n"
        ."Available Email Clients: ".join(", ", @ags).".";
      }
      elsif ($err =~ /Can't load .*? for module TREC::/) {
        $advice = "Perhaps a required shared library or dll isn't installed where expected";
      }
      elsif ($err =~ /Can't locate .*? in \@INC/) {
        $advice = "Perhaps a module that TREC::$email_client requires hasn't been fully installed";
      }
      Carp::croak("install_email_client($email_client) failed: $err$advice\n");
    }
#     warn("install_email_client 2\n");
    my $ec_dir;
    my $trec_ver;
    {#need this so I can use no strict below
       no strict 'refs';
       (my $ec_file = $ec_class) =~ s/::/\//g;
       $trec_ver = ${"$ec_class\::VERSION"} || "undef";
       $ec_dir   = substr($INC{qq($ec_file.pm)},0,(length($INC{qq($ec_file.pm)})-3));
    }
    # --- do some behind-the-scenes checks and setups on the driver
    $class->_setup_email_client($ec_class);
    $trech = eval { $ec_class->client($attr || {}) }; #call to called by Test::Class::Rig::TREC::<fs_name>
    # $trech->_debug('install_email_client',"install_email_client: $ec_class version $trec_ver loaded from  loaded from $ec_dir.pm!")
      # if ($class->TRACE() >= 1);
    unless ($trech && ref $trech && !$@) {
      my $advice = "";
      $@ ||= "$ec_class->Email Cleint didn't return a handle";
      # catch people on case in-sensitive systems using the wrong case
      $advice = "\nPerhaps the capitalisation of Test::Class::TREC '$email_client' isn't right."
        if $@ =~ /locate object method/;
      Carp::croak("$ec_class initialisation failed: $@$advice");
    }
    $Test::Class::Rig::installed_trec{$email_client} = $trech;
    return $trech;
  }
  *email_client = \&install_email_client;
{ package #hide from PAUSE Test::Class::Rig::common
  Test::Class::Rig::common;
   @Test::Class::Rig::common::ISA       = ('Test::Class::Rig');
   @Test::Class::Rig::trd::ISA          = ('Test::Class::Rig::common');
   @Test::Class::Rig::tra::ISA          = ('Test::Class::Rig::common');
   @Test::Class::Rig::trec::ISA          = ('Test::Class::Rig::common');
   @Test::Class::Rig::tref::ISA          = ('Test::Class::Rig::common');
   @Test::Class::Rig::rig::ISA          = ('Test::Class::Rig::common');
   @Test::Class::Rig::case::ISA         = ('Test::Class::Rig::common');
   @Test::Class::Rig::requirements::ISA = ('Test::Class::Rig::common');
   @Test::Class::Rig::tests::ISA        = ('Test::Class::Rig::common');
   @Test::Class::Rig::defaults::ISA     = ('Test::Class::Rig::common');
   Test::Class::Rig::common->attributes({SCALARS=>[qw(TRACE agent rig RaiseError PrintError PrintWarn)],
                                          HASHES=>[qw(TRACE_FLAGS)]});
}
# generic TIEHASH default methods:
  sub FETCH {
    my($h,$key)= @_;
    my $v = $h->{$key};
    return $v if defined $v;
    if ($key =~ /^NAME_.c$/) {
      my $cols = $h->FETCH('NAME');
      return
        unless $cols;
      my @lcols = map { lc $_ } @$cols;
      $h->{NAME_lc} = \@lcols;
      my @ucols = map { uc $_ } @$cols;
      $h->{NAME_uc} = \@ucols;
      return $h->FETCH($key);
    }
    if ($key =~ /^NAME.*_hash$/) {
      my $i=0;
      for my $c(@{$h->FETCH('NAME')||[]}) {
         $h->{'NAME_hash'}->{$c}    = $i;
         $h->{'NAME_lc_hash'}->{"\L$c"} = $i;
         $h->{'NAME_uc_hash'}->{"\U$c"} = $i;
         $i++;
      }
      return $h->{$key};
    }
    if (!defined $v && !exists $h->{$key}) {
      return ($h->FETCH('TaintIn') && $h->FETCH('TaintOut'))
        if $key eq'Taint';
      return (1==0)
        if $is_flag_attribute{$key}; # return perl-style sv_no, not undef
      return [] if $key eq 'ChildHandles' && $HAS_WEAKEN;
      if ($key eq 'Type') {
        return "trd"
          if $h->isa('Test::Class::Rig::trd');
        Carp::carp( sprintf "Can't determine Type for %s",$h );
      }
      if (!$is_valid_attribute{$key} and $key =~ m/^[A-Z]/) {
        local $^W; # hide undef warnings
        Carp::carp( sprintf "Can't get %s->{%s}: unrecognised attribute (@{[ %$h ]})",$h,$key )
      }
    }
    return $v;
  }
  sub STORE {
    my ($h,$key,$value) = @_;
    if ($key eq 'Debug') {
      $h->TRACE($value);
      return 1;
    }
    elsif (!$is_valid_attribute{$key} && $key =~ /^[A-Z]/ && !exists $h->{$key}) {
      Carp::carp(sprintf "Can't set %s->{%s}: unrecognised attribute or invalid value %s",
      $h,$key,$value);
    }
    $h->{$key} = $is_flag_attribute{$key} ? !!$value : $value;
    return 1;
  }
  sub FIRSTKEY { }
  sub NEXTKEY  { }
  sub EXISTS   { defined($_[0]->FETCH($_[1])) } # XXX undef?
  sub CLEAR    { Carp::carp "Can't CLEAR $_[0] (Test::Class::Rig)" }
{ package #hide from PAUSE Test::Class::Rig::tests
  Test::Class::Rig::tests;
  @Test::Class::Rig::tests::ISA = qw(Test::Class::Rig::common );
  Test::Class::Rig::tests->attributes({SCALARS=>[qw(tag)],
                                      # HAHSES=>[qw(no_locator)]
                                      });
  use strict;
  #use AutoLoader 'AUTOLOAD';
  # use Test::Builder;
   use Test::More;
  # use Data::Dumper;
 
  
  our $AUTOLOAD;
  my $Test = Test::Builder->new;
  $Test->exported_to(__PACKAGE__);
  my %comparator = (
    is       => 'is_eq',
    isnt     => 'isnt_eq',
    like     => 'like',
    unlike   => 'unlike',
  );
  sub setup {
    #just here to stop error if TRD has no setup method
  }
  sub teardown  {
    #just here to stop error if TRD has no teardown  method
  }
  sub shutdown {
    #just here to stop error if TRD has no shutdown method
  }
  sub startup {
    #just here to stop error if TRD has no startup method
  }
  
  
  sub AUTOLOAD {
    my $name = $AUTOLOAD;
    $name =~ s/.*:://;
    #warn("Test::Class::Rig::tests->AUTOLOAD name = $name\n");
    return if $name eq 'DESTROY';
    
    my $self  = $_[0];
    my ($opt) = @_;
    my $sub;
   
    if ($name =~ /(\w+)_(is|isnt|like|unlike)$/i) {
       my $getter = "$1";
       my $comparator = $comparator{lc $2};
       $sub = sub {
                my $self = shift;
                my( $opt ) = @_;
                Carp::croak(ref($self)."->$name  option 'HASH' must have an expected key!")
                   unless( $opt->{expected});
                #warn("\n\n locator self=".$self." locator=".$locator." str=".$str." name=".$name."\n");
                 #diag "Test::Class::TRA::Mechanize running $getter (@_[1..$#_])"
                 #   if $self->{verbose};
                #$name = "$getter, $locator, '$str'"
                #    if $self->{default_names} and !defined $name;
                no strict 'refs';
                my $rc = $Test->$comparator( $self->$getter($opt),  $opt->{expected}, $opt->{name} );
                return $rc;
              };
    }
    elsif ($name =~ /(\w+?)_?ok$/i) {
        my $cmd = $1;
        # make a subroutine for ok() around the the command
        $sub = sub {
          #  my = shift;
            my ($self ,$opt ) = @_;
            Carp::croak(ref($self)."->$name  must have an option 'HASH'!")
              if( ref($opt) ne 'HASH' );
            local $Test::Builder::Level = $Test::Builder::Level + 1;
            my $rc = '';
            eval { $rc = $self->$cmd( $opt ) };
            die $@ if $@ and $@ =~ /Can't locate object method/;
            diag($@) if $@;
            $rc = ok( $rc, $opt->{name} );
            return $rc;
        };
    }
    # jump directly to the new subroutine, avoiding an extra frame stack
    if ($sub) {
        no strict 'refs';
        *{$AUTOLOAD} = $sub;
        goto &$AUTOLOAD;
    }
    else {
        my $sel = 'Test::Class::Rig::tests';
        my $sub = "${sel}::${name}";
        goto &$sub if exists &$sub;
        my ($package, $filename, $line) = caller;
        die qq(Can't locate object method "$name" via package ")
            . __PACKAGE__
            . qq(" (also tried "$sel") at $filename line $line\n);
    }
  }

  sub no_locator {
    my $self   = shift;
    my $method = shift;
    $self->no_locator({login=>1});
    #my %no_locator = $self->no_locator();
    return $self->exists_no_locator($method);
  }
  
  sub ping {
    warn("rig tests ping\n");
  }
}  #Test::Class::Rig::tests
{ package #hide from PAUSE Test::Class::Rig::case
  Test::Class::Rig::case;
  @Test::Class::Rig::case::ISA = qw(Test::Class::Rig::common Test::Class );
  use Test::Class;
  use strict;
  Test::Class::Rig::case->attributes({SCALARS=>[qw( name default requirement)],
                                      ARRAYS =>[qw(results)],
                                      HASHES =>[qw(requirements)],} );
  __PACKAGE__->add_testinfo('z_tcrc_shutdown', shutdown  =>"no_plan");
  __PACKAGE__->add_testinfo('z_tcrc_teardown', teardown  =>"no_plan");
  __PACKAGE__->add_testinfo('shutdown', shutdown  =>"no_plan");
  __PACKAGE__->add_testinfo('startup', shutdown  =>"no_plan");
  __PACKAGE__->add_testinfo('setup', shutdown  =>"no_plan");
  __PACKAGE__->add_testinfo('teardown', shutdown  =>"no_plan");
  # sub agent {
    # my $self = shift;
    # return $self->{rig}->agent();
  # }
  sub z_tcrc_shutdown  { #silly name I know but I want them to run last
     my $self   = shift;
     use Data::Dumper;
    # my @details = $self->builder->details;
     # my $name = $self->name();
     # my $result =  {$name=>\@details};
     # $self->rig()->push_results($result);
     # $self->builder->reset();
  }
  sub z_tcrc_teardown  { #silly name I know but I want them to run last
     my $self   = shift;
     my $tests =  $self->_test_info()->{$self->name()};
     my $method = $tests->{$self->current_method};
     use Data::Dumper;
     # my $junk = $self->_test_info();
     # warn("junk=".Dumper($junk));
     
     my @details = $self->builder->details;
     
     my $results = {$self->current_method()=>{details=>\@details,
                                       expected_tests=>$method->num_tests()
                                             },
                   };
     
     $self->push_results($results);
     
     # my @results = $self->results();
     # warn("z_tcrc_teardown =".Dumper(\@results));
     $self->rig()->push_raw_results(@details);
     $self->builder->reset();
     #warn($self->current_method()." details= ".Dumper(\@details));
  }
  sub _runtest {
    my $self = shift;
    $self->runtests();
  }
  
  sub ok ($;$) {
     my $self = shift;
     my( $test, $name ) = @_;
     my $tb = Test::More->builder;
     # warn("Test::Class::Rig::case ok self=".ref($self).", test=".$test.",name=".$name);
     return $tb->ok( $test, $name );
  }
  
} #Test::Class::Rig::case


{ package #hide from PAUSE Test::Class::Rig::requirement
  Test::Class::Rig::requirement;
  @Test::Class::Rig::requirement::ISA = qw(Test::Class::Rig::common);
  Test::Class::Rig::requirement->attributes({SCALARS=>[qw(name id description attributes)],} );
  use strict;
} #Test::Class::Rig::requirements;

{ package #hide from PAUSE Test::Class::Rig::defaul
  Test::Class::Rig::default;
  @Test::Class::Rig::default::ISA = qw(Test::Class::Rig::common);
  Test::Class::Rig::default->attributes({SCALARS=>[qw(name id description attributes)],} );
  use strict;
} #Test::Class::Rig::default;

{ package #hide from PAUSE Test::Class::Rig::rig
  Test::Class::Rig::rig;	# ====== TEST Rig DRIVER Empyt======
  @Test::Class::Rig::rig::ISA = qw(Test::Class::Rig::common);
  Test::Class::Rig::rig->attributes({SCALARS=>[qw(test_count dbd_name db_user_id db_name db_password db_schema db_attributes
                                                       email_client email_file_system email_user email_password db_date_time_format
                                                       defaults_dir requirements_dir config_dir case_dir default_dir
                                                       url_base runs _current_run time_zone browser )],
                                   ORDERED_HASHES=>[qw(defaults cases requirements )],
                                           ARRAYS=>[qw(browsers results raw_results)],
                                       });
  #use base qw(Test::Class);
  use strict;
  my %Added_to_INC;
  sub _init_email_client {
    my $self = shift;
    # my $email_client= undef;
    # if ($self->email_client_type() eq 'IMAP') {
       # $email_client = Net::IMAP::Simple->new($self->email_file_system(),[port => 143]) ||
          # die "Unable to connect to IMAP: Net::IMAP::Simple::errstr\n";
        # if ($self->TRACE() >= 3){
           # warn("init_email_client server=".$self->email_file_system()." email user=".$self->email_user().", password=".$self->email_password()."\n");
        # }
        # if (!$email_client->login($self->email_user(),$self->email_password())){
           # print STDERR "Login failed: " . $email_client->errstr . "\n";
           # exit(64);
        # }
    # }
   # $self->email_client($email_client);
  }

  sub runtest {
    
    my $self = shift;
    my ($case_name,$opt) = @_;
    
    my $test = $self->cases($case_name);
    Carp::croak("Fatal Error". ref($self)."->runtest()!! case $case_name not found")
      unless($test);
      
    
    if (exists($opt->{browser_index})) {
      
      my $browser = $self->browsers($opt->{browser_index});
      
      Carp::croak("Fatal Error". ref($self)."->runtest()!! browser index=".$opt->{browser_index}." not found!")
        unless($browser);
      
      $self->browser($browser);
      
    }

    if (exists($opt->{default_index})) {
      
      my $default = $self->defaults($opt->{default_index});
      
      Carp::croak("Fatal Error". ref($self)."->runtest()!! defaults index=".$opt->{default_index}." not found!")
        unless($default);
    
      $test->default($default);
    
    }
  
    $test->_runtest();
    my @details = $test->results();
   
    my $name = $test->name();
 
    my $result =  {$name=>{details =>\@details,
                           browser =>$self->browser(),
                           defaults=>$test->default()->name(),
                          }
                  };
    $test->results([]);
    $self->push_results($result);
 
  }

  sub runtests {
    
    my $self = shift;
    my ($opt) = @_;
    
    
    for(my $run=1; $run <= $self->runs(); $run++){
      
      $self->_current_run($run);
      
      foreach my $test ($self->cases()){
        
        foreach my $browser ($self->browsers()){
          
          $self->browser($browser);
          
          foreach my $defaults ($self->defaults()){
            
            $test->defaults($defaults);
            $test->_runtest();
            
          }
       }
      }
    }
  }
  
  sub _new_requirement {# called by Test::Class::TRD::<drivername>::rig
    my $rig = shift;
    my ($attr) = @_;
    my $imp_class = $rig->{ImplementorClass}
       or Carp::croak("Test::Class::Rig::rig::_new_requirement ".ref($rig)." has no ImplementorClass");
    substr($imp_class,-5,5) = '::requirement';
    my $requirement = $imp_class->new($attr);
    return $requirement;
  }
  sub _new_default {# called by Test::Class::TRD::<drivername>::rig
    my ($rig, $attr) = @_;
    my $imp_class = $rig->{ImplementorClass}
       or Carp::croak("Test::Class::Rig::rig::_new_default ".ref($rig)." has no ImplementorClass");
    substr($imp_class,-5,5) = '::default';
    my $default = $imp_class->new($attr);
    return $default;
  }
  sub _new_agent { # called by Test::Class::Rig::TRA::<agentname>::tra::load()
    my ($trah,$attr) = @_;
    my $imp_class = $trah->{ImplementorClass}
       or Carp::croak("Test::Class::Rig::rig->_new_agent $trah has no ImplementorClass");
    my $agent_class = $trah->{AgentClass}
       or Carp::croak("Test::Class::Rig::rig->_new_agent $trah has no AgentClass");
    {
       no strict 'refs';
       # push(@{"${agent_class}::ISA"},$imp_class)
          # unless UNIVERSAL::isa($agent_class, $imp_class);
       push(@{"${imp_class}::ISA"},$agent_class)
          unless UNIVERSAL::isa($imp_class, $agent_class);
       #warn("\n\n agent ISA==".Dumper(\@{"${imp_class}::ISA"}));
       #warn("\n\n agent ISA==".Dumper(\@{"${agent_class}::ISA"}));
    }
    eval "require $agent_class"; ## no critic
    my $agent= $imp_class->new($attr);
    return $agent;
  }
  sub _new_case { # called by Test::Class::TRD::<drivername>::rig
    my ($rig, $case_class) = @_;
    my $imp_class = $rig->{ImplementorClass}
      or Carp::croak("Test::Class::TRD::rig->_new_case '$rig' has no ImplementorClass");
    my $case = $case_class->Test::Class::new();
    $case->agent($rig->agent());
    $case->name($case_class);
    $case->{rig}=$rig;
    $case->rig($rig);
    $rig->cases({$case_class=>$case});
    return $case;
  }
  sub _is_test_class { #well at least it looks like it
    my $self = shift;
    my ( $file ) = @_;
    # By default, we only care about ...Test.pm files
    if ($file =~ /Test\.pm$/) {
        return 1;
    }
    return;
  }
  sub _load_cases {
    my $self = shift;
    my $haveFileSpec = eval { require File::Spec };
    my ($file, $dir ) = @_;
    $file =~ s{\.pm$}{};             # remove .pm extension
    $file =~ s{\\}{/}g;              # to make win32 happy
    $dir  =~ s{\\}{/}g;              # to make win32 happy
    $file =~ s/^$dir//;
    my $_package = join '::' => grep $_ => File::Spec->splitdir( $file );
    # untaint that puppy!
    # warn("_load_cases $dir _package=$_package\n");
    my ( $package ) = $_package =~ /^([[:word:]]+(?:::[[:word:]]+)*)$/;
    # Filter out bad classes (mainly this means things in .svn and similar)
    return unless defined $package;
    # warn("_load_cases package=$package\n");
    my $imp_class = $self->{ImplementorClass}
      or Carp::croak("Test::Class::Rig::_load_cases ".ref($self)." has no ImplementorClass");
    my $app_class = ref $self;
    substr($app_class,-5,5)='';
    {
      no strict 'refs';
      push @{"${package}::ISA"},$app_class."::case"
        unless UNIVERSAL::isa($package, $app_class."::case");
      push @{"${package}::ISA"},$app_class."::tests"
        unless UNIVERSAL::isa($package, $app_class."::tests");
      push @{"${package}::ISA"},"Test::Class"
        unless UNIVERSAL::isa($package, "Test::Class");
      push @{"${package}::ISA"},"Test::More"
        unless UNIVERSAL::isa($package, "Test::More");
    }
    unshift @INC => $dir;# unless $self->Added_to_INC{ $dir }++;
    eval "require $package"; ## no critic
    if ($@) {
      my $err = $@;
      my $advice = "Test case $package will not be run!!\n\n";
      warn("\n\n Load of test $package failed: \n   Error=$err \n $advice\n");
    }
    else {
        $self->load_case($package);	   #call to Test::Class::Rig::<drivername>::rig
    #   warn("_load_cases loaded=".ref($test_h));
    #   exit;
    }
  }
  sub import_requirments {
    my $self = shift;
    my ( $path ) = @_;
    my $haveFileSpec = eval { require File::Spec };
    $path|=$self->requirements_dir();
    # Open the directory.
    opendir (DIR, $path)
        or die "Unable to open $path: $!";
    my @files = grep { !/^\.{1,2}$/ } readdir (DIR);
    # Close the directory.
    closedir (DIR);
    @files = map { $path . '/' . $_ } @files;
    for (@files) {
      # If the file is a directory
      if (-d $_) {
        # using a new directory we just found.
        $self->import_requirments($_);
      }
      elsif(lc(substr($_,-4,4)) eq '.yml') {
        my ($volume,$directories,$file) = File::Spec->splitpath( $_);
        my $attributes;
#warn("laoding yml".$file."\n");
        eval{
          $attributes = $self->_yaml_to_hash($file,$volume.$directories);
        };
        if ($@) {
          my $err = $@;
          warn("Requirments yml file $file did not Parse \n\n it failed with this error \n\n$err\n");
        }
        my $requirement  = $self->load_requirement($attributes);  #call to trd dirver
        $self->requirements({$requirement->id()=>$requirement});
      }
    }
  }
 sub load_defaults {
    my $self = shift;
    my ($dir,$file)=@_;
    $dir  |=$self->defaults_dir();
    $file |="defaults.yml";
    my $defaults = $self->_yaml_to_hash($file,$dir);
    use Data::Dumper;
    foreach my $default_attr (@{$defaults->{defaults}}){
      my $default=$self->load_default($default_attr);   #call to TRD
      $self->defaults({$default->id()=>$default});
    }
 }
  sub import_cases {
    my $self = shift;
    my ( $path ) = @_;
    my $haveFileSpec = eval { require File::Spec };
    $path|=$self->case_dir();
    # Open the directory.
    opendir (DIR, $path)
        or die "Unable to open $path: $!";
    my @files = grep { !/^\.{1,2}$/ } readdir (DIR);
    # Close the directory.
    closedir (DIR);
    @files = map { $path . '/' . $_ } @files;
    for (@files) {
      # If the file is a directory
      if (-d $_) {
        # using a new directory we just found.
        $self->import_cases($_);
      } elsif($self->_is_test_class($_)) {
        my ($volume,$directories,$file) = File::Spec->splitpath( $_);
        $self->_load_cases($file,$directories);
      }
    }
    #warn("done import cases");
  }
sub load_config {
    my $self = shift;
    my ($dir,$file)=@_;
    $dir  |=$self->config_dir();
    $file |="config.yml";
       my $config_hash = $self->_yaml_to_hash($file,$dir);
    # use Data::Dumper;
    #
    # warn(Dumper($config_hash));
    $self->db_attributes($config_hash->{db_attributes});
    $self->dbd_name($config_hash->{dbd_name});
    $self->db_user_id($config_hash->{db}->{user_id});
    $self->db_password($config_hash->{db}->{password});
    $self->db_name($config_hash->{db}->{database_name});
    $self->db_schema($config_hash->{db}->{schema});
    $self->db_date_time_format($config_hash->{db}->{date_time_format});
   # $self->email_client_type($config_hash->{email_file_system}->{type});
   # $self->email_file_system($config_hash->{email_file_system}->{server});
    $self->email_user($config_hash->{email_file_system}->{user});
    $self->email_password($config_hash->{email_file_system}->{password});
    $self->_init_email_client()
      if ($self->email_file_system());
    $self->url_base($config_hash->{url_base});
     $self->browsers($config_hash->{browser});
     if (ref($config_hash->{browser}) ne 'array'){ #must allways be an array ref
       $self->browsers([$config_hash->{browser}]);
     }
    $self->runs($config_hash->{runs});
    $self->time_zone($config_hash->{time_zone});
    return 1;
  }
  sub set_default_dirs {
    my $self  = shift;
    my ($dir) = @_;
    $self->config_dir($dir);
    $self->default_dir($dir);
    $self->defaults_dir($dir.'\Defaults');
    $self->requirements_dir($dir.'\Requirements');
    $self->case_dir($dir.'\Cases');
  }
}  # Test::Class::Rig::rig
{ package #hide from PAUSE Test::Class::Rig::trd
    Test::Class::Rig::trd;	# ====== TEST Rig DRIVER Empty======
  @Test::Class::Rig::trd::ISA = qw(Test::Class::Rig::common);
  Test::Class::Rig::trd->attributes({SCALARS=>[qw(driver_dir)]});
  use strict;
#really nothing here as it just a holder for the load method which is implimented in the TRD
  sub ping_trd {
   print "trd all this does is say ping\n\n";
  }

} # Test::Class::Rig::trd
{ package #hide from PAUSE Test::Class::Rig::tra
   Test::Class::Rig::tra;	# ====== TEST Rig AGENT Empty======
   @Test::Class::Rig::tra::ISA = qw(Test::Class::Rig::common);
  # Test::Class::Rig::tra->attributes({SCALARS=>[qw(dir agent)]});
  #really just a holder for the cross agent methods
   use strict;
   use Data::Dumper;
  sub ping {
   print "tra all this does is say ping\n\n";
  }
  sub fill_field {
    my $self = shift;
    my ($locator,$type,$value,$caption) = @_;
    # if ($self->TRACE<=3){
       # warn("fill_field  field=".Dumper($locator).
                         # ", value=".$value.
                         # ", Caption=".$caption.
                         # ", type=".$type."\n");
    # }
    if (($type eq 'text') || ($type eq 'textarea') || ($type eq 'password') ){
      $self->type($locator, $value," Completed Field:".$caption);
    }
    elsif ($type eq 'select') {
      warn("Test::Class::Rig::tra->Select this\n");
       $self->select($locator,$value,"  Completed Field:".$caption );
    }
    # elsif($type eq 'TYPE_LOOKUP'){
      # $agent->select_ok("//select[\@name='".$field."']",$value," Selected value of ".$caption);
    # }
    # elsif($type eq 'TYPE_CHECK_HASH'){
      # if ($value){
        # $tra->click_ok($field,"Checked ".$caption);
      # }
    # }
    return 1;
  }
  sub fill_form {
    my $self = shift;
    my ($fields) = @_;
    foreach my $field (@{$fields}) {
      $self->fill_field($field->{locator},$field->{type},$field->{value},$field->{caption});
    }
    return 1;
  }
} # Test::Class::Rig::tra
{ package #hide from PAUSE Test::Class::Rig::trec
   Test::Class::Rig::trec;	# ====== TEST Rig emial client Empty======
   @Test::Class::Rig::trec::ISA = qw(Test::Class::Rig::common);
   use strict;
   use Data::Dumper;
  sub ping_trec {
   print "trec all this does is say ping\n\n";
  }
} # Test::Class::Rig::tref
{ package #hide from PAUSE Test::Class::Rig::tref
   Test::Class::Rig::tref;	# ====== TEST Rig emial file system Empty======
   @Test::Class::Rig::tref::ISA = qw(Test::Class::Rig::common);
   use strict;
   use Data::Dumper;
  sub ping_tref {
   print "tref all this does is say ping\n\n";
  }
} # Test::Class::Rig::trec
1;

=pod
=head1 Name
Test::Class::Rig - Create a whole new world of testng
=head1 SYNOPSIS
  use Test::Class::Rig;
  my $rig = Test::Class::Rig->load("AMC","Mechanize",{test=>1});
  $rig->runtests();
  
=head1 DESCRIPTION

Test::Class::Rig as the name implies is a 'Test Rig' based on the very popular
Test::Class. Specifiaclly it was created for testing web based applications but
it can be used to test any type of software. Test::Class::Rig defines an entire
suite of agents, objects, methods, and attributes that create consisteint
interface for testing across many platforms and applications. It is important
to remember that Test::Class::Rig an API or 'Glue' layer for one or more
'Driver' modules.  The  'Driver' modules consists of custom 'methods',
 'tests', 'cases', 'defaults' and 'requirements' for each application to be
tested.

=head2 Architecture of a Test::Class::Rig Application

                                                          +->+-------+
                            +-------------+               |  | Tests |
                            | Web Agent A |               |  +-------+
                            +-------------+               |
                            |                             +--+->+-------+
                            +-------------+               |  |  | Case1 |
                            | Web Agent Z |               |  |  +-------+
                            +-------------+               |  +->+-------+
                            |                  +----------+     | CaseN |
                           +-+                 | Driver A |     +-------+
                           |T|                 +----------+  
Perl                 +-+   |e|                 |          +--+->+--------------+
script               |A|   |s|                 |          |  |  | requirement1 |
using             -->|P|-->|t|-----------------+          |  |  +--------------+
Test::Class::Rig     |I|   |:|                 |          |  +->+--------------+
                     +-+   |R|                 |          |     | requirementN |
                           |i|                 |          |     +--------------+
                           |g|                 |          |   
                           +-+                 |          +------+->+----------+
                            |                  |                 |  | default1 |
                            +---------------+  +---------------+ |  +----------+
                            | Email Agent A |  | Other Drivers | +->+----------+
                            +---------------+  +---------------+    | defaultN |
                            |                                       +----------+
                            +---------------+
                            | Email Agent Z |
                            +---------------+
                            |
                            +-----+
                            | DBI |
                            +-----+

Test::Class::Rig (TCR) "dispatches" the method calls to the appropriate
driver for actual execution. The TCR is also responsible for the dynamic
loading of Agents, DBI, providing default implementations for methods, and
some test specific duties. Each driver (TRD) contains all the extra bits that
may be needed to fully test an application. One can create custom attributes
and subroutines for each of the objects within a driver.

Web and Email agents are normally only authored as generic libraries so authors
of TRDs normally need not worrry about the differing agents.

=head1 Backgound
Test::Class::Rig your handy-dandy web-site/web-application test system!
Ever write up a selenium test for a site get it working and then have the
requirments change?

Ever write up 600 lines of the selenium code only to relaize that you
can't reuse any of it for the next site you have to test it with Mechanize?

Ever write up a dozen tests and get them working then have another user role
come down the design pipe?

Well have no fear Test::CLass::Rig is here!!

I first started using Test::Class a few years ago and being familiar with JUnit
I found it very useful.  I did dicover that I was re-writeing the same code 
over and over again for each new application I was testing.  So I started to 
munge together a number of my favorite modules and Test::CLass::Rig was the 
result.

=head2 Isn't this just a rehash of Test::Class?
Well in a word yes. It does give you much more ability to reuse test cases between
applications.  Testing the Login into a web site is a good example of how
I can use the same test case between a number web sites. My 
Test::Class NormalTest.pm remains unchanged between the applictions all 
that changes is the TRD, Requirements and Defaults for each web site. The test 
stays the same.

As well you can change up which web 'Agent' to use and you can switch between
email agents as well. Finally I added in DBI as part of the whole so you will
always have that handy if needed.

It also eliminates the need to use Test::Class::Load as your test classes are 
automtically loaded for you.

You can also specify a large number of config options to control how your 
Test::CLass::Rig works.

You also can create methods that are available to all of your test classes
in a Test::CLass::Rig::Driver, as a bonus these functios can also be used as test.
 So no need to inclued packages or rewite code over and over again.
 
Think of it as Test::Class with alot more code options at your fingertips.

=head2 Testing and Software
It does not matter what methodolgy you use to create your software, Agile,
Waterfall, Big Pot of Mud, or Itertive. Testing can never completely identify
all the defects within software. The most you can expect is a criticism or
comparison of the software with some sort of set of requirements.  When the
expected results do not match the results returned from the software then you
might have a problem. TestRig allows you to links up a set of tests with any
number of differeing requirments to allow you set up a 'Requirements' based
testing system.

=head3 Requirements Based Testing
One of the key thoughs behind Test::CLass::Rig is that it is compatable with this 
style of testing.

=head4 Define Test Completion Criteria. 
The test effort has specific, quantitative and qualitative goals. Testing is 
completed only when the goals have been reached (e.g., testing is complete when 
all functional variations, fully sensitized for the detection of defects, 
and 100% of all statements and branch vectors have executed successfully 
in single run or set of runs with no code changes in between).

=head4 Design Test Cases. 
Logical test cases are defined by five characteristics:

  1) the initial state of the system prior to executing the test, 
  2) the data in the data base, 
  3) the inputs, 
  4) the expected outputs, and 
  5) the final system state.

Test::CLass::Rig covers the first two and the final characteristics  by 
giving you DBI at your fingertips, the  third with the 'default' object that is
used to store this data. The fourth is coverd off with the 'requirment' object 
that is used to store this data.

=head4 Build Test Cases. 
There are two parts needed to build test cases from logical test cases: 
  1) creating the necessary data, and 
  2) building the components to support testing 
  (e.g., build the navigation to get to the portion of the program being tested).

Test::CLass::Rig covers this off by letting you create custom methods for each
Driver. As well Test::Class::Rig supports any number of web agents, Email Client
and Email file system.  

=head4. Execute Tests. 
It almonst goes without saysing the Test::CLass::Rig will execute the test-case 
steps against the system being tested and record the results. Test::CLass::Rig
impliments Test::More so no need now to import it into each of you test modules.

=head2 It Test::CLass::Rig for me.
If you have a working .t test system don't bother. If you have a working Test::Class
you might want to.  

If you are limited in which web agent you can use and some-one has writted a TRA
for it then go for it. 

Like Test::Class if you are distributing you code it will cause much more trouble
to you uses as they have yet another load of modules to install.

=head2 Notation and Conventions

The following conventions are used in this document:
  TR    Shorthand for Test::CLass::Rig
  TRD   Shorthand for Test::Class::Rig Driver
  TRA   Shorthand for Test::Class::Rig Agent (or more properly Web Agent)
  TRC   Shorthand for Test::Class::Rig Case 
  TREC  Shorthand for Test::Class::Rig Email Client
  TREF  Shorthand for Test::Class::Rig Email File Systme Agent
  TRR   Shorthand for Test::Class::Rig Requirements
  TRDF  Shorthand for Test::Class::Rig Defaults    
  
  $rig  Rig handle object
  $tra  Agent (web) handle object 
  $trd  Driver handle object
  $trec Email client handle object
  $tref Email file system handle object
  
=head2 Outline Usage
To use TR the first thing you must do is create a TRD and all its tests.
That is a little out of scope for now so we will assume one is ready to go.

  use Test::CLass::Rig;
  use strict;
  
(The use strict; isn't required but is strongly recommended.)

They you will need to 'load' your TRD to get a handle for it from TR.

  my $rig = Test::Class::Rig->load("Google",{agent=>"Selenium",email_client=>'Sender'});

What will happen is TR will load in your TRD, the TRCs, the TRRs and TRDF files 
it finds and loades in the "Selenium" web agent and the 'Sender' email client.

You can now do some testing.

  $rig->runtest("Home");
  my @results = $rig->results();
  my @raw_results = $rig->raw_results();
  
  $rig->runtests();
  
Thats about it.

=head2 General Interface Rules & Caveats

Oddly enough you will have to be familiar with http://search.cpan.org/~adie/Test-Class-0.38/lib/Test/Class.pm 
Test::Class to use TR. If you do not know how to use it I suggest you head
there and do some reading and come back later.

The default file format for TR is YAML.  There is nothing stopping you from useing
other formats you will just have to add the ability to read them into your own
TRD. 

=head2 Naming Conventions and Name Space

The Test::Class::Rig package and all packages below it (Test::Class::Rig::*) are
reserved for use by the TR. Extensions and related modules use the 
Test::Class::Rigx:: namespace, though I have not seen any yet. 

  Package names beginning with TRD:: are reserved for use by TR drivers drivers. 
  Package names beginning with TRA:: are reserved for use by TR web agent drivers. 
  Package names beginning with TREC:: are reserved for use by TR email client drivers. 
  Package names beginning with TREF:: are reserved for use by TR email file system` drivers. 
  
All environment variables used by the TR begin with "TR_".
All environment variables used by the TRDs begin with "TRD_".
All environment variables used by the TRAs begin with "TRA_".
All environment variables used by the TRECs begin with "TREC_".
All environment variables used by the TREFs begin with "TREF_".


The letter case used for attribute names is significant and plays an important 
part in the portability of TR scripts. The case of the attribute name is used to
 signify who defined the meaning of that name and its values.

  Case of name  Has a meaning defined by
  ------------  ------------------------
  UPPER_CASE    Standards, e.g.,  X/Open, ISO SQL92 etc (portable)
  MixedCase     TR,TRA,TREC,TREF API (portable), underscores are not used.
  lower_case    TRD specific (non-portable)

It is of the utmost importance that TRA, TREC and TREF developers only use 
MixedCase attribute names when defining private attributes. 

Private attribute names must be prefixed with the TRA, TREC or TREF name or 
suitable abbreviation (e.g., "Mec_" for Mechanize, "Imap_" for IMAP).

It is of the utmost importance that Driver developers only use lowercase 
attribute names when defining private attributes. 
Private attribute names must be prefixed with the driver name or 
suitable abbreviation (e.g., "goog_" for Google etc).

=head1 The Rig Package and Class
In this section, we cover the TR class methods, utility functions, and the 
dynamic attributes associated with all TR handles.

=head2 Rig Class Methods

The following methods are provided by the TR class:

=head3 available_agents

  @array = Test::Class::Rig->available_agents;
  @array = Test::Class::Rig->available_agents($quite);
  
Returns a list of all available TRA web agents by searching for TRA::* 
modules through the directories in @INC. By default, a warning is given 
if some drivers are hidden by others of the same name in earlier directories. 
Passing a true value for $quiet will inhibit the warning.

=head3 Available_drivers

  @array = Test::Class::Rig->available_drivers;
  @array = Test::Class::Rig->available_drivers($quite);
  
Returns a list of all available TRD Drivers by searching for TRD::* 
modules through the directories in @INC. By default, a warning is given 
if some drivers are hidden by others of the same name in earlier directories. 
Passing a true value for $quiet will inhibit the warning.

=head3 Available_email_clients

  @array = Test::Class::Rig->available_email_clients;
  @array = Test::Class::Rig->available_email_clients($quite);
  
Returns a list of all available TREC Email Clients by searching for TREC::* 
modules through the directories in @INC. By default, a warning is given 
if some drivers are hidden by others of the same name in earlier directories. 
Passing a true value for $quiet will inhibit the warning.

=head3 Available_email_file_systems

  @array = Test::Class::Rig->available_email_file_systems''
  @array = Test::Class::Rig->available_email_file_systems($quite);
  
Returns a list of all available TREF Email File Systems by searching for TREF::* 
modules through the directories in @INC. By default, a warning is given 
if some drivers are hidden by others of the same name in earlier directories. 
Passing a true value for $quiet will inhibit the warning.

=head3 Connect

  my $dbh = $rig->connect();
  my $dbh = $rig->connect({db_attributes=>{ PrintError => 0,
                              AutoCommit => 0}});
                              
Returns a DBI connection handle using the connection attributes supplied via the 
Config file or any you may wish to overide via a Hashref.

=head3 Install_agent

  my $tra = $rig->install_agent("Mechanize",{https_off=>1});
  
Somtimes you may need another TRA handy. This funcion allows you to load that
TRA in the system.  It will be loacal to the current method but will inharet
attributes from the present $rig handle. It will not load into the $rig's or $trd's
agent attribute. Croaks on error.

=head3 Install_driver

  my $trd = $rig->install_driver("Yahoo",{no_cache=>1});
  
Somtimes you may need another TRD handy. This funcion allows you to load that
TRD in the system.  It will be loacal to the current method but will inharet
attributes from the present $rig handle. Useful for getting a set of TRDF or
TRR handles that might span more than one TRD. Croaks on error.

=head3 Install_email_client

  my $trec = $rig->install_email_client("SENDER",{user_id =>'xx',
                                                 password=>'qq'});
  
Somtimes you may need another TREC handy. This funcion allows you to load that
TREC in the system.  It will be local to the current method but will inharet
attributes from the present $rig handle. It will not load into the $rig's or $trd's
email_client attribute. Croaks on error.

=head3 Install_email_file_system

  my $tref = $rig->install_email_file_system("SENDER",{user_id =>'xx',
                                                 password=>'qq'});
  
Somtimes you may need another TREF handy. This funcion allows you to load that
TREF in the system.  It will be local to the current method but will inharet
attributes from the present $rig handle. It will not load into the $rig's or $trd's
email_file_system attribute. Croaks on error.

=head3 Installed_agents

  my %agents = $rig->installed_agents();

Returns a list of web agent name and agent handle pairs for all agents 'installed' 
(loaded) into the current process. The agent name does not include the 'TRA::' prefix.

To get a list of all agents available in your perl installation you can use 
"available_agents".

=head3 Installed_drivers

  my %drivers = $rig->installed_drivers();

Returns a list of driver name and driver handle pairs for all drivers 'installed' 
(loaded) into the current process. The driver name does not include the 'TRD::' prefix.

To get a list of all drivers available in your perl installation you can use 
"available_drivers".

=head3 Installed_email_clients

  my %clientrs = $rig->installed_email_clients();

Returns a list of email clients name and client handle pairs for all email clients
 'installed' (loaded) into the current process. The driver name does not include 
 the 'TREC::' prefix.

To get a list of all drivers available in your perl installation you can use 
"available_email_clients".

=head3 Installed_email_file_systems

  my %file_systems = $rig->installed_email_file_systems();

Returns a list of email file systems name and file systems handle pairs for 
all email file systems 'installed' (loaded) into the current process. The driver
 name does not include  the 'TREF::' prefix.

To get a list of all drivers available in your perl installation you can use 
"available_email_file_systems".

=head3 Installed_versions

  Test::Class::Rig->installed_versions;
  @ary  = Test::Class::Rig->installed_versions;
  $hash = Test::Class::Rig->installed_versions;

Calls available_drivers() and attempts to load each of them in turn using 
install_driver(). For each load that succeeds the driver name and version number
 are added to a hash.  When called in array context the list of successfully 
 loaded drivers is returned (without the 'TRD::' prefix). When called in scalar 
 context an extra entry for the TR is added and a reference to the hash is 
 returned.
 
 When called in a void context the installed_versions() method will print out a 
 formatted list of the hash contents, one per line, along with some other 
 information about the TR version and OS.
 
 Due to the potentially high memory cost and unknown risks of loading in an 
 unknown number of drivers that just happen to be installed on the system, 
 this method is not recommended for general use. Use available_drivers() instead.

The installed_versions() method is primarily intended as a quick way to see from
 the command line what's installed. For example:
 
   perl -MTest::Class::Rig -e 'Test::Class::Rig->installed_versions'
   
=head3 Load
 
    my $rig = Test::Class::Rig->load("Google",{agent=>"Selenium",email_client=>'Sender'});
    my $rig = Test::Class::Rig->load("DBtests",{});
    
Creates the rig you want to test with.  It take three params as follows;  

=head 4 $driver
The name of the TRD you want to load.  This is the base name of the class, i.e.
 without the 'Test::Class::TRD::' prefix.
    
=head4 $agents
A hash reference with up to three parts

  {agent=>'Selenium',
   email_client=>'Imap',
   email_file_system=>'Imap_Files'}
   
This loads in the differing agents you want to start your TR with.

=head4 $attributes
A hash reference that you can use to pass optional params to you agents and the 
TRD. Be careful when using this one as you do not want the options for one agent
to overide the ones of another.

Load is very resource intesive and sould only be used once in a script.  What a 
load does is to go out a load up all the parts of you TRD.  So it will first 
create a TR with your TRD then install any agents you asked for, next it will 
load any TRDF objects (default data) that are present in or under the 'defaults'
path, then it will import any TRC objects (test cases) that are  in or under the
 'case'  path, finally it will import any TRR objects (requirment data) that are
 in or under the 'requirements'  path.
 
It returns a handle to you TRD.

=head3 Visit_handles

  $rig->visit_handles( $coderef );
  $rig->visit_handles( $coderef, $info );

Where $coderef is a reference to a subroutine and $info is an arbitrary value 
which, if undefined, defaults to a reference to an empty hash. Returns $info.

For each installed TRD  handle, if any, $coderef is invoked as:

  $coderef->($driver_handle, $info);

If the execution of $coderef returns a true value then "visit_child_handles" is 
called on that child handle and passed the returned value as $info.

For example:

  my $info = $rig->visit_child_handles(sub {
      my ($h, $info) = @_;
      ++$info->{ $h->{Type} }; # count types of handles (dr/db/st)
      return $info; # visit kids
  });
  
See also "visit_child_handles".

=head1 Methods Common to all Handles
=head3 Agent 

    my $agent =  $rig->agent();
    
    my $tra = $rig->install_agent("Mechanize",{https_off=>1});
    $rig->agent($tra);
  
Sets or Gets the current TRA (agent) handle;

=head3 TRACE 

    my $debug_level =  $rig->TRACE();
    
    $rig->TRACE(4);
  
Sets or Gets the current TRACE level. A number less than 15. 

The TR has a powerful debuging mechanism built in. It enables you to see what's
going on 'behind the scenes', both within the TR and the drivers you're using.
Trace Settings

Which details are written to the trace output is controlled by a combination of 
a trace level, an integer from 0 to 15, and a set of trace flags that are either
 on or off. Together these are known as the trace settings.

Trace levels are as follows:

  0 - Trace disabled.
  1 - Trace top-level TR method calls returning with results or errors.
  2 - As above, adding tracing of top-level method entry with parameters.
  3 - As above, adding some high-level information from the driver
      and some internal information from the TR.
  4 - As above, adding more detailed information from the driver.
      This is the first level to trace all the rows being fetched.
  5 to 15 - As above but with more and more internal information.
  
=head3 TRACE_FLAG

  $rig->TRACE_FLAG('ALL');
  $rig->TRACE_FLAG('TRD');
      
      
Sets or Gets the current TRACE_FLAG. 

The Trace flag is used to enable tracing of specific activities within the TRD and
 drivers. The TRD defines some trace flags and drivers can define others. 
 TRD trace flag names begin with a capital letter and driver specific names 
 begin with a lowercase letter, as usual.

Currently the TRD defines these trace flags:

  ALL - turn on all TRD and driver flags (not recommended)
  TRA - trace only Agent releated calls
  TRD - trace only Driver releated calls
  TRC - trace only Case releated calls
  TREC - trace only Email Client releated calls
  TREF - trace only Email File System calls
  TRR  - trace only Requirements releated calls
  TRDF - trace only Defaults releated calls

=head3 PrintError 
Not implimented yet.
=head3 PrintWarn
Not implimented yet.
=head3 RaiseError 
Not implimented yet.
=head3 Rig 

  my $rig = $trd->rig();
  
Sets or Gets the current TR handle. 

=head2 Rig Methods

=head3 default_dirs
=head3 import_cases
 loads in test cases for a directly structure imports the test cases$.  
 A test case is a xxxx_test.pm file  XML file

=head3 import_requirments
  Loads in requirements  YAML file
=head3 load_config
opens the Config.XML file and sets all of the default valuses present
=head3 load_defaults
opens the use_cases.XML file and reads in all of the USE_CASE records
=head3 runtests
=head3 runtest

=head3 set_default_dirs

 sets the following default dirs

  config_dir,
  default_dir
  defaults = 'default_dir'+\Defaults
  requirements_dir 'default_dir'+\Requirements
  case_dir  'default_dir'+\Test_Cases
  
   from the values found in the config.xml file
   The directory that test_rig will look for the requirements files

=head1 Rig Driver Handle Attributes
This section covers the methods and attributes associated with rig handles.

=head3 Scalar Attributes
These a single value attributes usually set via the config file and are the 
defaults for you rig.

=head4 browser

  $rig->browser("IE4"); 
  
Sets or gets the name of the browser that the web Agent will default to when when
running test cases. If applicapbe to the agent.  Some TRA can only use a single
browser type of have no concept of a browser.

=head4 case_dir

  $rig->case_dir("/system/ming/alltests"); 

Sets or gets the direcotry that the TRD will look for the 'test case' files. 

=head4 db_attributes

  $rig->db_attributes({dbd_verbose=>10, ora_session_mode => ORA_SYSDBA})
  
Sets or gets the special DBI attributes that are set at connection time.  These
are independant of TR.

=head4 config_dir

  $rig->config_dir("/system/ming"); 

Sets or gets the direcotry that the TRD will look for the 'config' file. 

=head4 db_date_time_format

  $rig->db_date_time_format("mm/yyyy/ddd");
  
Sets or gets the special DBI date format at connection time.  This value depends
on your DBD driver and is independant of TR.

=head4 db_name 

  $rig->db_name("xe:10002");
 
Sets or gets the 'database_name' portion of your DBI connection 'data_source' string.  
Some examples may look like this

  dbi:DriverName:database_name
  dbi:DriverName:database_name@hostname:port
  dbi:DriverName:database=database_name;host=hostname;port=port

This value depends on your DBD driver and is independant of TR.

=head4 db_password 

  $rig->db_password("OH_GOOD_GOD");
  
Sets or gets the pasword part of your DBI connction string. 

=head4 db_schema

  $rig->db_schema("DILL_pickel");
  
Sets or gets the  DB schema at connection time.  This value depends
on your DBD driver and is independant of TR. 

=head4 db_user_id 

  $rig->db_user_id("ME");
  
Sets or gets the user id part of your DBI connction string. 

=head4 dbd_name 

  $rig->dbd_name("Oracle");
 
Sets or gets the DriverName portion of your DBI connection 'data_source' string.  
Some examples may look like this

  dbi:DriverName:database_name
  dbi:DriverName:database_name@hostname:port
  dbi:DriverName:database=database_name;host=hostname;port=port

This value depends on your DBD driver and is independant of TR.

=head4 default_dir

  $rig->default_dir("/system/ming"); 

Sets or gets the default directory for all other paths that the TRD will look 
for the 'config','defaults','case','requirements' files. 

=head4 defaults_dir

  $rig->defaults_dir("/system/ming/defaults"); 

Sets or gets the direcotry that the TRD will look for the 'defaults' files. 

=head4 email_client 

  my $email =  $rig->email_client();
  
Sets or Gets the current TREC handle.

=head4 email_file_system 

  my $email =  $rig->email_file_system();
  
Sets or Gets the current TREF handle.

=head4 email_password 

  $rig->email_password("LOIG");
  
Sets or Gets the current TREC password to use when connecting.

=head4 email_user 

  $rig->email_user("Lokie111");
  
Sets or Gets the current TREC user id to use when connecting.

=head4 requirements_dir

  $rig->requirements_dir("/system/ming/requirments"); 

Sets or gets the direcotry that the TRD will look for the 'requirements' files. 

=head4 runs 

  $rig->runs(2);

Sets or get the numer of times the TRD inerate over the suite of tests.

=head4 test_count

  $rig->test_count();

Gets the total number of tests the TR has run.

=head4 time_zone 

  $rig->time_zone("EST");

Gets or sets the time zone used by TR. This is just a holer for a value you can 
use in your test suite. 

=head4 url_base 

  $rig->url_base(2);

Gets or sets the 'base' url for any requests made by the TRA.

=head3 Array Attributes
These list attributes are set by a TRD at load or during operations.

=head4 browsers

  my @browsers = $rig->browsers;

Gets or sets the list a list of browsers to test against. Some agents simulate or
use a specific browser. This attribute can be used to store a list of the browsers
to test against. This value can be set via the config file.

=head4 raw_results

  my @TAP = $rig->raw_results;
  
The results of any testing run in raw TAP format. 
See http://testanything.org/wiki/index.php/Main_Page for more detaials on TAP.

=head4 results 

  my @results = $rig->results;
  
The results of any testing run in raw TAP plus some extra bits from TR and anything
eles one might add in your TRD. 

=head3 Ordered List Attributes
These attributes are hashes that retain the order in which they where imputed. 
These are set at load by a TRD.

=head4 cases

  my %all_cases  = $rig->cases;
  my @all_cases  = $rig->values_cases();
  my @case_names = $rig->keys_cases();
  my $base_cases = $rig->cases(('start_up','log_in'));

Sets and gets an ordered hash of test objects that have be loaded into the TR 
via TRD. The key value is the Package name of the the case.

=head4 defaults

  my %all_defaults   = $rig->defaults;
  my @all_defaults   = $rig->values_defaults();
  my @default_names  = $rig->keys_defaults();
  my $defaults_for_base_cases = $rig->defaults(('start_up','log_in'));

Sets and gets an ordered hash of defaults objects that have be loaded into 
the TR via TRD. The key value is the 'name' property.

=head4 requirements

  my %all_requirements   = $rig->requirements;
  my @all_requirements   = $rig->values_requirements();
  my @requirement_names  = $rig->keys_requirements();
  my $requirements_for_base_cases = $rig->requirements(('start_up','log_in'));

Sets and gets an ordered hash of requirements objects that have be loaded into 
the TR via TRD. The key value is the 'name' property.


=head1 TR AGENT HANDLE OBJECTS 
This section lists the methods and attributes associated with TRA (web agent)
handles. It is up the the AGENT itself to implement all of the methods listed here.
So far only one "TRA::Selenium" meets the API standard. TRA are free to implement 
any other methods they want but the base below should be met in-order to provied
protability of testing suites.

=head2 SYNOPSIS

    
  my $tra = $rig->agent();

  $tra->start;
  $tra->open("http://www.google.com");
  $tra->type("q", "hello world");
  $tra->click("btnG");
  $tra->wait_for_page_to_load(5000);
  print $tra->get_title;
  $tra->stop;

or as a series of tests

  $tra->start_ok;
  $tra->open_ok("http://www.google.com");
  $tra->type_ok("q", "hello world");
  $tra->click_ok("btnG");
  $tra->wait_for_page_to_load(5000);
  print $tra->get_title;
  $tra->stop_ok;
  
=head2 DESCRIPTION

Each TRA will have its own setup.  For example if you want to use TRA::Selenium 
you need to have already downloaded and started the Selenium Server (The 
Selenium Server is a Java application.) to actully use the agent. 
 Others like TRA::Mechanize can work right out of the box. Each TRA should document
 its setup procedures.

=head2 Element Locators

Element Locators tell the TRA which HTML element a command refers to.

The format of a locator is:

=over

=item I<locatorType>B<=>I<argument>

=back

Each TRA should support the following strategies for locating elements or at 
least on of them:

=over

=item *

B<identifier>=I<id>: Select the element with the specified @id attribute. 
If no match isfound, select the first element whose @name attribute is I<id>.
(This is normally the default; see below.)

=item *

B<id>=I<id>:Select the element with the specified @id attribute.

=item *

B<name>=I<name>:Select the first element with the specified @name attribute.

=over

=item *

username

=item *

name=username

=back

The name may optionally be followed by one or more I<element-filters>, separated
 from the name by whitespace.  If the I<filterType> is not specified, B<value>
  is assumed.

=over

=item *

name=flavour value=chocolate

=back

=item *

B<dom>=I<javascriptExpression>: Find an element by evaluating the specified 
string.  This allows you to traverse the HTML Document ObjectModel using 
JavaScript if your TRA allows that.  Note that you must not return a value in 
this string; simply make it the last expression in the block.

=over

=item *

dom=document.forms['myForm'].myDropdown

=item *

dom=document.images[56]

=item *

dom=function foo() { return document.links[1]; }; foo();

=back

=item *

B<xpath>=I<xpathExpression>: Locate an element using an XPath expression.

=over

=item *

xpath=//img[@alt='The image alt text']

=item *

xpath=//table[@id='table1']//tr[4]/td[2]

=item *

xpath=//a[contains(@href,'#id1')]

=item *

xpath=//a[contains(@href,'#id1')]/@class

=item *

xpath=(//table[@class='stylee'])//th[text()='theHeaderText']/../td

=item *

xpath=//input[@name='name2' and @value='yes']

=item *

xpath=//*[text()="right"]

=back

=item *

B<link>=I<textPattern>:Select the link (anchor) element which contains text 
matching thespecified I<pattern>.

=over

=item *

link=The link text

=back

=item *

B<css>=I<cssSelectorSyntax>:Select the element using css selectors. 
Please refer to http://www.w3.org/TR/REC-CSS2/selector.html (CSS2 selectors), 
http://www.w3.org/TR/2001/CR-css3-selectors-20011113/ (CSS3 selectors) for more 
information. 

=over

=item *

css=a[href="#id3"]

=item *

css=span#firstChild + span

=back

Currently the css selector locator supports all css1, css2 and css3 selectors 
except namespace in css3, some pseudo classes(:nth-of-type, :nth-last-of-type, 
:first-of-type, :last-of-type, :only-of-type, :visited, :hover, :active, :focus,
 :indeterminate) and pseudo elements(::first-line, ::first-letter, ::selection, 
 ::before, ::after).

=item *

B<ui>=I<uiSpecifierString>:Locate an element by resolving the UI specifier 
string to another locator, and evaluating it. 

=over

=item *

ui=loginPages::loginButton()

=item *

ui=settingsPages::toggle(label=Hide Email)

=item *

ui=forumPages::postBody(index=2)//a[2]

=back

=back

Without an explicit locator prefix, the TRA should use one of the following 
defaultstrategies:

=over

=item *

B<dom>, for locators starting with "document."

=item *

B<xpath>, for locators starting with "//"

=item *

B<identifier>, otherwise

=back

=head2 Element Filters

Element filters can be used with a locator to refine a list of candidate 
elements.  They are currently used only in the 'name' element-locator.

Filters look much like locators, ie.

=over

=item I<filterType>B<=>I<argument>

=back

Supported element-filters are:

=over

=item B<value=>I<valuePattern>

Matches elements based on their values.  This is particularly useful for 
refining a list of similarly-named toggle-buttons.

=item B<index=>I<index>

Selects a single element based on its position in the list (offset from zero).

=back

=head2 String-match Patterns

Various Pattern syntaxes are available for matching string values:

=over

=item *

B<glob:>I<pattern>:Match a string against a "glob" (aka "wildmat") pattern. 
"Glob" is akind of limited regular-expression syntax typically used in 
command-lineshells. In a glob pattern, "*" represents any sequence of 
characters, and "?" represents any single character. Glob patterns match 
against the entirestring.

=item *

B<regexp:>I<regexp>:Match a string using a regular-expression. 

=item *

B<regexpi:>I<regexpi>:Match a string using a case-insensitive regular-expression.

=item *

B<exact:>I<string>:Match a string exactly, verbatim, without any of that fancy 
wildcard stuff.

=back

If no pattern prefix is specified, the TRA should assumes that it's a "glob"pattern.

For commands that return multiple values (such as verifySelectOptions),the 
string being matched is a comma-separated list of the return values, where both
 commas and backslashes in the values are backslash-escaped. When providing a 
 pattern, the optional matching syntax (i.e. glob,regexp, etc.) is specified 
 once, as usual, at the beginning of thepattern.

=head2 METHODS

The following methods are available plus each of these there is a corresponding 
<command>_ok method that checks the return value (open_ok, click_ok, type_ok).

For each getter (get_title, ...) methond there are four autogenerated methods 
(<getter>_is, <getter>_isnt, <getter>_like, <getter>_unlike) to check the value 
of the attribute.


=item $agent->pause($timeout)

Waits $timeout milliseconds (default: 1 second)

=item $tra->click($locator)

Clicks on a link, button, checkbox or radio button. If the click actioncauses a
 new page to load (like a link usually does), callwaitForPageToLoad.

=over

=item $locator is an element locator

=back

=item $tra->double_click($locator)

Double clicks on a link, button, checkbox or radio button. If the double click 
action causes a new page to load (like a link usually does), callwaitForPageToLoad.

=over

=item $locator is an element locator

=back

=item $tra->context_menu($locator)

Simulates opening the context menu for the specified element (as might happen if
 the user "right-clicked" on the element).

=over

=item $locator is an element locator

=back

=item $tra->click_at($locator, $coord_string)

Clicks on a link, button, checkbox or radio button. If the click actioncauses a 
new page to load (like a link usually does), callwaitForPageToLoad.

=over

=item $locator is an element locator

=item $coord_string is specifies the x,y position (i.e. - 10,20) of the mouse 
     event relative to the element returned by the locator.

=back

=item $tra->double_click_at($locator, $coord_string)

Doubleclicks on a link, button, checkbox or radio button. If the action causes a
 new page to load (like a link usually does), callwaitForPageToLoad.

=over

=item $locator is an element locator

=item $coord_string is specifies the x,y position (i.e. - 10,20) of the mouse
      event relative to the element returned by the locator.

=back

=item $tra->context_menu_at($locator, $coord_string)

Simulates opening the context menu for the specified element (as might happen 
if the user "right-clicked" on the element).

=over

=item $locator is an element locator

=item $coord_string is specifies the x,y position (i.e. - 10,20) of the mouse 
     event relative to the element returned by the locator.

=back

=item $tra->fire_event($locator, $event_name)

Explicitly simulate an event, to trigger the corresponding "onI<event>"handler.

=over

=item $locator is an element locator

=item $event_name is the event name, e.g. "focus" or "blur"

=back

=item $tra->focus($locator)

Move the focus to the specified element; for example, if the element is an input 
field, move the cursor to that field.

=over

=item $locator is an element locator

=back

=item $tra->key_press($locator, $key_sequence)

Simulates a user pressing and releasing a key.

=over

=item $locator is an element locator

=item $key_sequence is Either be a string("\" followed by the numeric keycode  
of the key to be pressed, normally the ASCII value of that key), or a single  
character. For example: "w", "\119".

=back

=item $tra->shift_key_down()

Press the shift key and hold it down until doShiftUp() is called or a new page 
is loaded.

=item $tra->shift_key_up()

Release the shift key.

=item $tra->meta_key_down()

Press the meta key and hold it down until doMetaUp() is called or a new page is
 loaded.

=item $tra->meta_key_up()

Release the meta key.

=item $tra->alt_key_down()

Press the alt key and hold it down until doAltUp() is called or a new page is 
loaded.

=item $tra->alt_key_up()

Release the alt key.

=item $tra->control_key_down()

Press the control key and hold it down until doControlUp() is called or a new 
page is loaded.

=item $tra->control_key_up()

Release the control key.

=item $tra->key_down($locator, $key_sequence)

Simulates a user pressing a key (without releasing it yet).

=over

=item $locator is an element locator

=item $key_sequence is Either be a string("\" followed by the numeric keycode  
of the key to be pressed, normally the ASCII value of that key), or a single
  character. For example: "w", "\119".

=back

=item $tra->key_up($locator, $key_sequence)

Simulates a user releasing a key.

=over

=item $locator is an element locator

=item $key_sequence is Either be a string("\" followed by the numeric keycode
  of the key to be pressed, normally the ASCII value of that key), or a single 
   character. For example: "w", "\119".

=back

=item $tra->mouse_over($locator)

Simulates a user hovering a mouse over the specified element.

=over

=item $locator is an element locator

=back

=item $tra->mouse_out($locator)

Simulates a user moving the mouse pointer away from the specified element.

=over

=item $locator is an element locator

=back

=item $tra->mouse_down($locator)

Simulates a user pressing the left mouse button (without releasing it yet) on
the specified element.

=over

=item $locator is an element locator

=back

=item $tra->mouse_down_right($locator)

Simulates a user pressing the right mouse button (without releasing it yet) on
the specified element.

=over

=item $locator is an element locator

=back

=item $tra->mouse_down_at($locator, $coord_string)

Simulates a user pressing the left mouse button (without releasing it yet) at
the specified location.

=over

=item $locator is an element locator

=item $coord_string is specifies the x,y position (i.e. - 10,20) of the mouse 
     event relative to the element returned by the locator.

=back

=item $tra->mouse_down_right_at($locator, $coord_string)

Simulates a user pressing the right mouse button (without releasing it yet) at
the specified location.

=over

=item $locator is an element locator

=item $coord_string is specifies the x,y position (i.e. - 10,20) of the mouse
      event relative to the element returned by the locator.

=back

=item $tra->mouse_up($locator)

Simulates the event that occurs when the user releases the mouse button 
(i.e., stopsholding the button down) on the specified element.

=over

=item $locator is an element locator

=back

=item $tra->mouse_up_right($locator)

Simulates the event that occurs when the user releases the right mouse button 
(i.e., stopsholding the button down) on the specified element.

=over

=item $locator is an element locator

=back

=item $tra->mouse_up_at($locator, $coord_string)

Simulates the event that occurs when the user releases the mouse 
button (i.e., stopsholding the button down) at the specified location.

=over

=item $locator is an element locator

=item $coord_string is specifies the x,y position (i.e. - 10,20) of the mouse 
     event relative to the element returned by the locator.

=back

=item $tra->mouse_up_right_at($locator, $coord_string)

Simulates the event that occurs when the user releases the right mouse button
 (i.e., stopsholding the button down) at the specified location.

=over

=item $locator is an element locator

=item $coord_string is specifies the x,y position (i.e. - 10,20) of the mouse
      event relative to the element returned by the locator.

=back

=item $tra->mouse_move($locator)

Simulates a user pressing the mouse button (without releasing it yet) on
the specified element.

=over

=item $locator is an element locator

=back

=item $tra->mouse_move_at($locator, $coord_string)

Simulates a user pressing the mouse button (without releasing it yet) on
the specified element.

=over

=item $locator is an element locator

=item $coord_string is specifies the x,y position (i.e. - 10,20) of the mouse
      event relative to the element returned by the locator.

=back

=item $tra->type($locator, $value)

Sets the value of an input field, as though you typed it in.
Can also be used to set the value of combo boxes, check boxes, etc. In these
 cases,value should be the value of the option selected, not the visible text.

=over

=item $locator is an element locator

=item $value is the value to type

=back

=item $tra->type_keys($locator, $value)

Simulates keystroke events on the specified element, as though you typed the
 value key-by-key. This is a convenience method for calling keyDown, keyUp, 
 keyPress for every character in the specified string;this is useful for 
 dynamic UI widgets (like auto-completing combo boxes) that require explicit 
 key events.

Unlike the simple "type" command, which forces the specified value into the page
 directly, this commandmay or may not have any visible effect, even in cases 
 where typing keys would normally have a visible effect.For example, if you use 
 "typeKeys" on a form element, you may or may not see the results of what you 
 typed inthe field.

In some cases, you may need to use the simple "type" command to set the value of
 the field and then the "typeKeys" command tosend the keystroke events 
 corresponding to what you just typed.

=over

=item $locator is an element locator

=item $value is the value to type

=back

=item $tra->set_speed($value)

Set execution speed, by default, there is no such delay, i.e.,the 
delay is 0 milliseconds.

=over

=item $value is the number of milliseconds to pause after operation

=back

=item $tra->get_speed()

Get execution speed, by default, there is no such delay, i.e.,the delay is
 0 milliseconds.See also setSpeed.

=over

=item Returns the execution speed in milliseconds.

=back

=item $tra->check($locator)

Check a toggle-button (checkbox/radio)

=over

=item $locator is an element locator

=back

=item $tra->uncheck($locator)

Uncheck a toggle-button (checkbox/radio)

=over

=item $locator is an element locator

=back

=item $tra->select($select_locator, $option_locator)

Select an option from a drop-down using an option locator.
Option locators provide different ways of specifying options of an HTML
Select element (e.g. for selecting a specific option, or for assertingthat the 
selected option satisfies a specification). There are severalforms of Select 
Option Locator.

=over

=item *

B<label>=I<labelPattern>:matches options based on their labels, i.e. the visible
 text. (Thisis the default.)

=over

=item *

label=regexp:^[Oo]ther

=back

=item *

B<value>=I<valuePattern>:matches options based on their values.

=over

=item *

value=other

=back

=item *

B<id>=I<id>:matches options based on their ids.

=over

=item *

id=option1

=back

=item *

B<index>=I<index>:matches an option based on its index (offset from zero).

=over

=item *

index=2

=back

=back

If no option locator prefix is provided, the default behaviour is to match on 
B<label>.

=over

=item $select_locator is an element locator identifying a drop-down menu

=item $option_locator is an option locator (a label by default)

=back

=item $tra->add_selection($locator, $option_locator)

Add a selection to the set of selected options in a multi-select element using 
an option locator.@see #doSelect for details of option locators

=over

=item $locator is an element locator identifying a multi-select box

=item $option_locator is an option locator (a label by default)

=back

=item $tra->remove_selection($locator, $option_locator)

Remove a selection from the set of selected options in a multi-select element 
using an option locator.@see #doSelect for details of option locators

=over

=item $locator is an element locator identifying a multi-select box

=item $option_locator is an option locator (a label by default)

=back

=item $tra->remove_all_selections($locator)

Unselects all of the selected options in a multi-select element.

=over

=item $locator is an element locator identifying a multi-select box

=back

=item $tra->submit($form_locator)

Submit the specified form. This is particularly useful for forms withoutsubmit 
buttons, e.g. single-input "Search" forms.

=over

=item $form_locator is an element locator for the form you want to submit

=back

=item $tra->open($url)

Opens an URL in the test frame. This accepts both relative and absolute URLs.
The "open" command waits for the page to load before proceeding,ie. the "AndWait"
 suffix is implicit.
 
=over

=item $url is the URL to open; may be relative or absolute

=back

=item $tra->open_window($url, $window_id)

Opens a popup window (if a window with that ID isn't already open). After opening
 the window, you'll need to select it using the selectWindowcommand.

=over

=item $url is the URL to open, which can be blank

=item $window_id is the JavaScript window ID of the window to select

=back

=item $tra->select_window($window_id)

Selects a popup window using a window locator; once a popup window has been 
selected, allcommands go to that window. To select the main window again, use null
as the target.
Window locators provide different ways of specifying the window object:by title, 
by internal JavaScript "name," or by JavaScript variable.

=over

=item *

B<title>=I<My Special Window>:Finds the window using the text that appears in 
the title bar.  Be careful;two windows can share the same title.  If that happens,
 this locator willjust pick one.

=item *

B<name>=I<myWindow>:Finds the window using its internal JavaScript "name" property.
  This is the second parameter "windowName" passed to the JavaScript method 
  window.open(url, windowName, windowFeatures, replaceFlag)
  

=item *

B<var>=I<variableName>:Some pop-up windows are unnamed (anonymous), 
but are associated with a JavaScript variable name in the current application 
window, e.g. "window.foo = window.open(url);".  In those cases, you can open 
the window using"var=foo".

=back

If no window locator prefix is provided, the TRD should try to guess what you 
mean like this.

=over

=item $window_id is the JavaScript window ID of the window to select

=back

=item $tra->select_pop_up($window_id)

Simplifies the process of selecting a popup window (and does not offer
functionality beyond what C<selectWindow()> already provides).

=over

=item *

If C<windowID> is either not specified, or specified as "null", the first non-top
 window is selected. The top window is the onethat would be selected by 
 C<selectWindow()> without providing aC<windowID> . This should not be used when
  more than one popupwindow is in play.


=over

=item $window_id is an identifier for the popup window, which can take on a
                  number of different meanings

=back

=item $tra->deselect_pop_up()

Selects the main window. Functionally equivalent to using C<selectWindow()> and 
specifying no value forC<windowID>.

=item $tra->select_frame($locator)

Selects a frame within the current window.  (You may invoke this command multiple 
times to select nested frames.)  To select the parent frame, use"relative=parent" 
as a locator; to select the top frame, use "relative=top".  You can also select 
a frame by its 0-based index number; select the first frame with"index=0", or 
the third frame with "index=2".
You may also use a DOM expression to identify the frame you want directly, 
like this: C<dom=frames["main"].frames["subframe"]>

=over

=item $locator is an element locator identifying a frame or iframe

=back

=item $tra->get_whether_this_frame_match_frame_expression($current_frame_string, $target)

Determine whether current/locator identify the frame containing this running code.

=over

=item $current_frame_string is starting frame

=item $target is new frame (which might be relative to the current one)

=back

=over

=item Returns true if the new frame is this code's window

=back

=item $tra->get_whether_this_window_match_window_expression($current_window_string, $target)

Determine whether currentWindowString plus target identify the window containing
 this running code. 

=over

=item $current_window_string is starting window

=item $target is new window (which might be relative to the current one, e.g., "_parent")

=back

=over

=item Returns true if the new window is this code's window

=back

=item $tra->wait_for_pop_up($window_id, $timeout)

Waits for a popup window to appear and load up.

=over

=item $window_id is the JavaScript window "name" of the window that will appear 
(not the text of the title bar)

If unspecified, or specified as "null", this command will wait for the first 
non-top window to appear (don't rely on this if you are working with multiple 
popups  simultaneously).

=item $timeout is a timeout in milliseconds, after which the action will return
 with an error.   If this value is not specified, the default TRD 
 timeout will be used. See the setTimeout() command.

=back

=item $tra->go_back()

Simulates the user clicking the "back" button on their browser.

=item $tra->refresh()

Simulates the user clicking the "Refresh" button on their browser.

=item $tra->close()

Simulates the user clicking the "close" button in the titlebar of a popup window
 or tab.

=over

=item $tra->get_location()

Gets the absolute URL of the current page.

=item Returns the absolute URL of the current page

=back

=item $tra->get_title()

Gets the title of the current page.

=over

=item Returns the title of the current page

=back

=item $tra->get_body_text()

Gets the entire text of the page.

=over

=item Returns the entire text of the page

=back

=item $tra->get_value($locator)

Gets the (whitespace-trimmed) value of an input field (or anything else with a 
value parameter).For checkbox/radio elements, the value will be "on" or "off" 
depending on whether the element is checked or not.

=over

=item $locator is an element locator

=back

=over

=item Returns the element value, or "on/off" for checkbox/radio elements

=back

=item $tra->get_text($locator)

Gets the text of an element. This works for any element that contains text. 
This command uses either the textContent (Mozilla-like browsers) or the
innerText (IE-like browsers) of the element, which is the rendered text shown
to the user.

=over

=item $locator is an element locator

=back

=over

=item Returns the text of the element

=back

=item $tra->highlight($locator)

Briefly changes the backgroundColor of the specified element yellow.  
Useful for debugging.

=over

=item $locator is an element locator

=back

=item $tra->is_checked($locator)

Gets whether a toggle-button (checkbox/radio) is checked.  Fails if the specified 
element doesn't exist or isn't a toggle-button.

=over

=item $locator is an element locator pointing to a checkbox or radio button

=back

=over

=item Returns true if the checkbox is checked, false otherwise

=back

=item $tra->get_table($table_cell_address)

Gets the text from a cell of a table. The cellAddress syntaxtableLocator.row.column, 
where row and column start at 0.

=over

=item $table_cell_address is a cell address, e.g. "foo.1.4"

=back

=over

=item Returns the text from the specified cell

=back

=item $tra->get_selected_labels($select_locator)

Gets all option labels (visible text) for selected options in the specified 
select or multi-select element.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns an array of all selected option labels in the specified select drop-down

=back

=item $tra->get_selected_label($select_locator)

Gets option label (visible text) for selected option in the specified select element.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns the selected option label in the specified select drop-down

=back

=item $tra->get_selected_values($select_locator)

Gets all option values (value attributes) for selected options in the specified 
select or multi-select element.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns an array of all selected option values in the specified select drop-down

=back

=item $tra->get_selected_value($select_locator)

Gets option value (value attribute) for selected option in the specified select element.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns the selected option value in the specified select drop-down

=back

=item $tra->get_selected_indexes($select_locator)

Gets all option indexes (option number, starting at 0) for selected options in 
the specified select or multi-select element.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns an array of all selected option indexes in the specified select 
drop-down

=back

=item $tra->get_selected_index($select_locator)

Gets option index (option number, starting at 0) for selected option in the 
specified select element.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns the selected option index in the specified select drop-down

=back

=item $tra->get_selected_ids($select_locator)

Gets all option element IDs for selected options in the specified select or 
multi-select element.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns an array of all selected option IDs in the specified select drop-down

=back

=item $tra->get_selected_id($select_locator)

Gets option element ID for selected option in the specified select element.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns the selected option ID in the specified select drop-down

=back

=item $tra->is_something_selected($select_locator)

Determines whether some option in a drop-down menu is selected.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns true if some option has been selected, false otherwise

=back

=item $tra->get_select_options($select_locator)

Gets all option labels in the specified select drop-down.

=over

=item $select_locator is an element locator identifying a drop-down menu

=back

=over

=item Returns an array of all option labels in the specified select drop-down

=back

=item $tra->get_attribute($attribute_locator)

Gets the value of an element attribute. The value of the attribute may differ 
across browsers (this is the case for the "style" attribute, forexample).

=over

=item $attribute_locator is an element locator followed by an @ sign and then 
the name of the attribute, e.g. "foo@bar"

=back

=over

=item Returns the value of the specified attribute

=back

=item $tra->is_text_present($pattern)

Verifies that the specified text pattern appears somewhere on the rendered page 
shown to the user.

=over

=item $pattern is a pattern to match with the text of the page

=back

=over

=item Returns true if the pattern matches the text, false otherwise

=back

=item $tra->is_element_present($locator)

Verifies that the specified element is somewhere on the page.

=over

=item $locator is an element locator

=back

=over

=item Returns true if the element is present, false otherwise

=back

=item $tra->is_visible($locator)

Determines if the specified element is visible. Anelement can be rendered 
invisible by setting the CSS "visibility" property to "hidden", or the "display" 
property to "none", either for the element itself or one if its ancestors.  
This method will fail if the element is not present.

=over

=item $locator is an element locator

=back

=over

=item Returns true if the specified element is visible, false otherwise

=back

=item $tra->is_editable($locator)

Determines whether the specified input element is editable, ie hasn't been 
disabled.This method will fail if the specified element isn't an input element.

=over

=item $locator is an element locator

=back

=over

=item Returns true if the input element is editable, false otherwise

=back

=item $tra->get_all_buttons()

Returns the IDs of all buttons on the page.
If a given button has no ID, it will appear as "" in this array.

=over

=item Returns the IDs of all buttons on the page

=back

=item $tra->get_all_links()

Returns the IDs of all links on the page.
If a given link has no ID, it will appear as "" in this array.

=over

=item Returns the IDs of all links on the page

=back

=item $tra->get_all_fields()

Returns the IDs of all input fields on the page.
If a given field has no ID, it will appear as "" in this array.

=over

=item Returns the IDs of all field on the page

=back

=item $tra->get_attribute_from_all_windows($attribute_name)

Returns an array of JavaScript property values from all known windows having one.

=over

=item $attribute_name is name of an attribute on the windows

=back

=over

=item Returns the set of values of this attribute from all known windows.

=back

=item $tra->dragdrop($locator, $movements_string)

deprecated - use dragAndDrop instead

=over

=item $locator is an element locator

=item $movements_string is offset in pixels from the current location to which 
the element should be moved, e.g., "+70,-300"

=back

=item $tra->set_mouse_speed($pixels)

Configure the number of pixels between "mousemove" events during dragAndDrop 
commands (default=10).  Setting this value to 0 means that we'll send a "mousemove" 
event to every single pixel in between the start location and the end location; 
that can be very slow, and may cause some browsers to force the JavaScript to 
timeout.

If the mouse speed is greater than the distance between the two dragged objects, 
we'lljust send one "mousemove" at the start location and then one final one at 
the end location.

=over

=item $pixels is the number of pixels between "mousemove" events

=back

=item $tra->get_mouse_speed()

Returns the number of pixels between "mousemove" events during dragAndDrop 
commands (default=10).

=over

=item Returns the number of pixels between "mousemove" events during dragAndDrop
commands (default=10)

=back

=item $tra->drag_and_drop($locator, $movements_string)

Drags an element a certain distance and then drops it

=over

=item $locator is an element locator

=item $movements_string is offset in pixels from the current location to which 
the element should be moved, e.g., "+70,-300"

=back

=item $tra->drag_and_drop_to_object($locator_of_object_to_be_dragged, $locator_of_drag_destination_object)

Drags an element and drops it on another element

=over

=item $locator_of_object_to_be_dragged is an element to be dragged

=item $locator_of_drag_destination_object is an element whose location 
(i.e., whose center-most pixel) will be the point where locatorOfObjectToBeDragged  
is dropped

=back

=item $tra->window_focus()

Gives focus to the currently selected window

=item $tra->window_maximize()

Resize currently selected window to take up the entire screen

=item $tra->get_all_window_ids()

Returns the IDs of all windows that the browser knows about in an array.

=over

=item Returns Array of identifiers of all windows that the browser knows about.

=back

=item $tra->get_all_window_names()

Returns the names of all windows that the browser knows about in an array.

=over

=item Returns Array of names of all windows that the browser knows about.

=back

=item $tra->get_all_window_titles()

Returns the titles of all windows that the browser knows about in an array.

=over

=item Returns Array of titles of all windows that the browser knows about.

=back

=item $tra->get_html_source()

Returns the entire HTML source between the opening andclosing "html" tags.

=over

=item Returns the entire HTML source

=back

=item $tra->set_cursor_position($locator, $position)

Moves the text cursor to the specified position in the given input element or 
textarea. This method will fail if the specified element isn't an input element 
or textarea.

=over

=item $locator is an element locator pointing to an input element or textarea

=item $position is the numerical position of the cursor in the field; position 
should be 0 to move the position to the beginning of the field.  You can also 
set the cursor to -1 to move it to the end of the field.

=back

=item $tra->get_element_index($locator)

Get the relative index of an element to its parent (starting from 0). The comment 
node and empty text node will be ignored.

=over

=item $locator is an element locator pointing to an element

=back

=over

=item Returns of relative index of the element to its parent (starting from 0)

=back

=item $tra->is_ordered($locator1, $locator2)

Check if these two elements have same parent and are ordered siblings in the DOM. 
Two same elements will not be considered ordered.

=over

=item $locator1 is an element locator pointing to the first element

=item $locator2 is an element locator pointing to the second element

=back

=over

=item Returns true if element1 is the previous sibling of element2, false otherwise

=back

=item $tra->get_element_position_left($locator)

Retrieves the horizontal position of an element

=over

=item $locator is an element locator pointing to an element OR an element itself

=back

=over

=item Returns of pixels from the edge of the frame.

=back

=item $tra->get_element_position_top($locator)

Retrieves the vertical position of an element

=over

=item $locator is an element locator pointing to an element OR an element itself

=back

=over

=item Returns of pixels from the edge of the frame.

=back

=item $tra->get_element_width($locator)

Retrieves the width of an element

=over

=item $locator is an element locator pointing to an element

=back

=over

=item Returns width of an element in pixels

=back

=item $tra->get_element_height($locator)

Retrieves the height of an element

=over

=item $locator is an element locator pointing to an element

=back

=over

=item Returns height of an element in pixels

=back

=item $tra->get_cursor_position($locator)

Retrieves the text cursor position in the given input element or textarea.
This method will fail if the specified element isn't an input element or textarea, 
or there is no cursor in the element.

=over

=item $locator is an element locator pointing to an input element or textarea

=back

=over

=item Returns the numerical position of the cursor in the field

=back

=item $tra->get_expression($expression)

Returns the specified expression.
This is useful because of JavaScript preprocessing. It is used to generate 
commands like assertExpression and waitForExpression.

=over

=item $expression is the value to return

=back

=over

=item Returns the value passed in

=back

=item $tra->get_xpath_count($xpath)

Returns the number of nodes that match the specified xpath, eg. "//table" would 
givethe number of tables.

=over

=item $xpath is the xpath expression to evaluate. do NOT wrap this expression in 
a 'count()' function; we will do that for you.

=back

=over

=item Returns the number of nodes that match the specified xpath

=back

=item $tra->assign_id($locator, $identifier)

Temporarily sets the "id" attribute of the specified element, so you can locate 
it in the futureusing its ID rather than a slow/complicated XPath.  
This ID will disappear once the page is reloaded.

=over

=item $locator is an element locator pointing to an element

=item $identifier is a string to be used as the ID of the specified element

=back

=item $tra->allow_native_xpath($allow)

Specifies whether the TRA should use the native in-browser implementation of 
XPath (if any native version is available)

=over

=item $allow is boolean, true means the TRA will use native XPath.

=back

=item $tra->ignore_attributes_without_value($ignore)

Specifies whether the TRA will ignore xpath attributes that have novalue, 
i.e. are the empty string, when using the non-native xpath evaluation engine. 

=over

=item $ignore is boolean, true means we'll ignore attributes without value at 
the expense of xpath "correctness".

=back

=item $tra->set_timeout($timeout)

Specifies the amount of time that the TRA will wait for actions to complete.
Actions that require waiting include "open" and the "waitFor*" actions.
The default timeout is 30 seconds.

=over

=item $timeout is a timeout in milliseconds, after which the action will return 
with an error

=back

=item $tra->wait_for_page_to_load($timeout)

Waits for a new page to load.

=over

=item $timeout is a timeout in milliseconds, after which this command will 
return with an error

=back

=item $tra->wait_for_frame_to_load($frame_address, $timeout)

Waits for a new frame to load.

=over

=item $frame_address is FrameAddress from the server side

=item $timeout is a timeout in milliseconds, after which this command will return 
with an error

=back

=item $tra->get_cookie()

Return all cookies of the current page under test.

=over

=item Returns all cookies of the current page under test

=back

=item $tra->get_cookie_by_name($name)

Returns the value of the cookie with the specified name, or throws an error if 
the cookie is not present.

=over

=item $name is the name of the cookie

=back

=over

=item Returns the value of the cookie

=back

=item $tra->is_cookie_present($name)

Returns true if a cookie with the specified name is present, or false otherwise.

=over

=item $name is the name of the cookie

=back

=over

=item Returns true if a cookie with the specified name is present, or false 
otherwise.

=back

=item $tra->create_cookie($name_value_pair, $options_string)

Create a new cookie whose path and domain are same with those of current page
under test, unless you specified a path for this cookie explicitly.

=over

=item $name_value_pair is name and value of the cookie in a format "name=value"

=item $options_string is options for the cookie. Currently supported options 
include 'path', 'max_age' and 'domain'. the optionsString's format is 
"path=/path/, max_age=60, domain=.foo.com".  The order of options are irrelevant,
the unit of the value of 'max_age' is second.  Note that specifying a domain 
that isn't a subset of the current domain will usually fail.

=back

=item $tra->delete_cookie($name, $options_string)

Delete a named cookie with specified path and domain.  Be careful; to delete a 
cookie, youn eed to delete it using the exact same path and domain that were 
used to create the cookie.  If the path is wrong, or the domain is wrong, the 
cookie simply won't be deleted.  Also note that specifying a domain that isn't a
subset of the current domain will usually fail.  Since there's no way to 
discover at runtime the original path and domain of a given cookie, we've added 
an option called 'recurse' to try all sub-domains of the current domain with all
paths that are a subset of the current path.  Beware; this option can be slow.  
In big-O notation, it operates in O(n*m) time, where n is the number of dots in 
the domain name and m is the number of slashes in the path.

=over

=item $name is the name of the cookie to be deleted

=item $options_string is options for the cookie. Currently supported options 
include 'path', 'domain' and 'recurse.' The optionsString's format is 
"path=/path/, domain=.foo.com, recurse=true".  The order of options are irrelevant. 
Note that specifying a domain that isn't a subset of the current domain will 
usually fail.

=back

=item $tra->delete_all_visible_cookies()

Calls deleteCookie with recurse=true on all cookies visible to the current page.
As noted on the documentation for deleteCookie, recurse=true can be much slower
than simply deleting the cookies using a known domain/path.

=item $tra->set_browser_log_level($log_level)

Sets the threshold for browser-side logging messages if the TRA can do this.

=over

=item $log_level is one of the following: "debug", "info", "warn", "error" or "off"

=back

=item $tra->add_location_strategy($strategy_name)

Defines a new function for the TRD to locate elements on the page.  

=over

=item *

locator: the string the user passed in

=item *

inWindow: the currently selected window

=item *

inDocument: the currently selected document

=back

The function must return null if the element can't be found.

=over

=item $strategy_name is the name of the strategy to define; this should use only   
letters [a-zA-Z] with no spaces or other punctuation.

=back

=item $tra->capture_entire_page_screenshot($filename, $kwargs)

Saves the entire contents of the current window canvas to a PNG file.
=over

=item $filename is the path to the file to persist the screenshot as. No filename
extension will be appended by default.  Directories will not be created if they 
do not exist, and an exception will be thrown, possibly by native code.

=item $kwargs is a kwargs string that modifies the way the screenshot is captured. 
Example: "background=#CCFFDD". Currently valid options:

=back

=item $tra->rollup($rollup_name, $kwargs)

Executes a command rollup, which is a series of commands with a unique name, and 
optionally arguments that control the generation of the set ofcommands. If any 
one of the rolled-up commands fails, the rollup is considered to have failed. 
Rollups may also contain nested rollups.

=over

=item $rollup_name is the name of the rollup command

=item $kwargs is keyword arguments string that influences how the rollup expands 
into commands

=back

=item $tra->use_xpath_library($library_name)

Allows choice of one of the available libraries.

=over

=item $library_name is name of the desired library. The number and choice of 
libraries is set by the individual TRDs.

=back

=item $tra->set_context($context)

Writes a message to the status bar and adds a note to the browser-sidelog.

=over

=item $context is the message to be sent to the browser

=back

=item $tra->attach_file($field_locator, $file_locator)

Sets a file input (upload) field to the file listed in fileLocator

=over

=item $field_locator is an element locator

=item $file_locator is a URL pointing to the specified file. Implimention of this 
function will be defined by the individual TRDs

=back

=item $tra->capture_screenshot($filename)

Captures a PNG screenshot to the specified file.

=over

=item $filename is the absolute path to the file to be written, e.g. "c:\blah\screenshot.png"

=back

=item $tra->capture_screenshot_to_string()

Capture a PNG screenshot.  It then returns the file as a base 64 encoded string.

=over

=item Returns The base 64 encoded string of the screen shot (PNG file)

=back

=item $tra->capture_entire_page_screenshot_to_string($kwargs)

Downloads a screenshot of the browser current window canvas to a based 64 encoded 
PNG file. The I<entire> windows canvas is captured, including parts rendered 
outside of the current view port.  Currently this only works in Mozilla and when 
running in chrome mode.

=over

=item $kwargs is A kwargs string that modifies the way the screenshot is captured. 
Example: "background=#CCFFDD". This may be useful to set for capturing screenshots 
of less-than-ideal layouts, for example where absolute positioning causes the 
calculation of the canvas dimension to fail and a black background is exposed  
(possibly obscuring black text).

=back

=over

=item Returns The base 64 encoded string of the page screenshot (PNG file)

=back

=item $tra->key_up_native($keycode)

Simulates a user releasing a key by sending a native operating system keystroke.

=over

=item $keycode is an integer keycode number corresponding to a java.awt.event.KeyEvent.

=back

=item $tra->key_press_native($keycode)

Simulates a user pressing and releasing a key by sending a native operating system 
keystroke. 

=over

=item $keycode is an integer keycode number corresponding to a java.awt.event.KeyEvent; 
note that Java keycodes are NOT the same thing as JavaScript keycodes!

=back

=item $tra->wait_for_text_present($text, $timeout)

Waits until $text is present in the html source

=item $tra->wait_for_element_present($locator, $timeout)

Waits until $locator is present

=item $tra->is_location($expected_location)

Verify the location of the current page ends with the expected location.
If an URL querystring is provided, this is checked as well.

=over

=item $expected_location is the location to match.

=back

=head2 Agent Methods

=head3 fill_field
=head3 fill_form

=head2 Email Client Methods

=head2 Email File System Methods

=head2 Case Attributes
=head3 Scalar Attributes
=head4 Default 
=head4 Name 
=head4 Requirement
=head3 Array Attributes
=head4 Results
=head3 Hash Attributes
=head4 Requirements
=head2 Case Methods
=head2 Requirement Attributes
=head3 Scalar Attributes
=head4 Attributes
=head4 Description
=head4 ID
=head4 Name 
=head2 Default Attributes
=head3 Scalar Attributes
=head4 Attributes
=head4 Description 
=head4 ID 
=head4 Name 


=head2 Requirement Properties
=head3 Array Properties
names
the names
  get_names
  set_names
ids
  get_ids
  set_ids
content
  get_content
  set_content
=head2  Test_case Propeties
=head3 Scalar Properties
use_case
The use case that the test case will run against
  get_use_case
  set_use_case
requirement
The requirment hash that the test case will run against
  get_requirement
  set_requirement
=head2 Use_Case Handel Methods
=head3 Scalar Properties
name
The name of the test case, This is preset to be the PACKAGE name of test.
  get_name
  set_name
id
The id of the test case
  get_id
  set_id
=head1 Checks Methods
=head1 on_correct_page
returns true if the TRA location is the same as the passed in URL
It takes one param
    1) a URL
  Usage
    if ($self->on_correct_page($url){
      
=head1 condtion_fields
  Iterates over the FIELD_NAMES array in the props and checkes for any fields that have {VALIDATION}->{CONDITIONS}
  properties.  It then tests for there
   It takes four params
    1) a valid Test::WWW::Selenium object
    2) a scalar field name for a "SUBMIT" Button
    3) a hash-ref of field properties
    4) a hash-ref of Key/Value pairs
  Usage
    $self->condtion_fields_ok($sel,'submit',{test=>[1,2,2]},{1=>'none',});
  The number of tests run is variable as it depends on the number of {VALIDATION}->{CONDITIONS} are on the form.

=head1 items_not_checked
  This sub iterates over a list of items and checks each to ensure the corresponding checkbox is not checked
  It takes three params
    1) a valid Test::WWW::Selenium object
    2) a scalar field name to test against. Must be a checkbox
    3) an array ref for values for the above field
  Usage
    $self->items_checked_ok($sel,'my_checkbox',[1,2,4,5,6,7]);
  The total number of test performed is ewual to the the number of items in the list param
  
=head1 items_checked_ok
  This sub iterates over a list of items and checks each to ensure the corresponding checkbox is checked
  It takes three params
    1) a valid Test::WWW::Selenium object
    2) a scalar field name to test against. Must be a checkbox
    3) an array ref for values for the above field
  Usage
    $self->items_checked_ok($sel,'my_checkbox',[1,2,4,5,6,7]);
  The total number of test performed is ewual to the the number of items in the list param
  
=head1 sleep_warn
  This sub will sleep perl. Usually used to sleep the system while awaiting a AJAX return
  It takes two params
    1) A Warning message
    2) The sleep time in seconds
  Usage
    $self->sleep_warn("I am bored so I will make you bored as well",1000);

=head1 select_list
  This is a generic test for checking displayed picklist. It takes three params
    1) a valid Test::WWW::Selenium object
    2) a scalar field name
    3) a hash-ref of Key/Value pairs
  Usage
    $self->pick_list($sel,'A_field',{1=>'me',2=>'you',3=>'us'})
  It does two (2) tests;
    1) Are there is the same # of items on the picklist as in the hash
    2) Are the Value items the same as the ones displayed on the picklist

=head1 form_default_state
This test ensures that a the present page form is in the default state.
It takes two params
  1) a valid Test::WWW::Selenium object
  2) the hasref of FIELDS for this form
Usage
  $self->form_is_default_state_ok($sel,$PROPS->{FIELDS});
The number of tests run depends on the number of property fields tested
Note:
  Each $PROPS->{FIELD} should have a DEFAULT value set.
  
=head1 fill_field
This sub attempts to fill in the passed field
It takes five params
  1) a valid Test::WWW::Selenium object
  2) a Scalar field name
  3) a Scalar Field type
  4) a Scalar Field value
  5) a Scalar Caption
Usage
  $self->fill_field_ok($sel,'p_title','TYPE_TEXT','This is a title','Title');
This sub will run 1 test
1) Did the field fill in correctly

=head1 fill_form
This sub attempts to complete a form from the values passed into it
It takes three params
  1) a valid Test::WWW::Selenium object
  2) a Hashref of field properties
  3) a Hashref of Filed values
Usage
  $self->fill_form($sel,$properties,$values);
The number of testes run is equal to the number of items in the FIELD_NAMES array of Field Properties hasref.

=head1 page_display_values
This sub checkes to ensure that the correct values are displayed on a Display.pl page
It takes four params
  1) a valid Test::WWW::Selenium object
  2) the current URL to be tested
  3) a Hashref of field properties
  4) a Hashref of Filed values
Usage
  $self->page_display_values($sel,$my_URL,$properties,$values);
The number of testes run is equal to the number of items in the FIELD_NAMES array of Field Properties hasref plus one for
the page itself.

=head1 field_value
This sub checks to ensure that the correct field values is correct.
It takes four params
  1) a valid Test::WWW::Selenium object
  2) a scalar field name
  3) a Hashref of the field properties
  4) the value to test against
Usage
  $self->field_value($sel,'p_desc',$props,"This should be a testfield value";
his sub will run 1 test
1) Is the field filled in correctly
=head1 form_values
This sub checks to ensure that the correct values are displayed on a form
It takes four params
  1) a valid Test::WWW::Selenium object
  2) a Hashref of field properties
  3) a Hashref of Filed values
Usage
  $self->form_values($sel,$properties,$values);
The number of testes run is equal to the number of items in the FIELD_NAMES array of Field Properties hashref

=head1 email_body_contains

Returns true if the body of  the email message contains the value.
It takes two params
  1) a valid Email::MIME message
  2) a Scalar value to test for
Usage
  $self->email_body_contains($message,'you3uck');
No test are run

=head1 get_email_header
Gets an email that matches a Key value
It takes two params
  1) a valid Email::MIME message
  2) a Key value to test for (must be a vaild Email::MIME header key)
Usage
  $self->get_email_header_key_value($message,'From');
No test are run

=head1 get_email_subject_to_from
This sub searches the INBOX of an imap for a email that matches the to and from address and subject values and returns it
It takes four params
  1) a valid Net::IMAP::Simple object
  2) a Scalar value for subject
  2) a Scalar value for the to address
  3) a Scalar value for the from address
Usage
  $self->get_email_subject_to_from($imap,'Is there anyone there','toast@hoor.com','you@test.com');
It does no tests

=head1 get_email_by_subject
This sub searches the INBOX of an imap for a email that matches the subject values and returns it
It takes four params
  1) a valid Net::IMAP::Simple object
  2) a Scalar value for subject
Usage
  $self->get_email_subject_to_from($imap,'Is there anyone there');
It does no tests
    =head1 imap_client
Returns the Net::IMAP::Simple client;

=head1 get_email_by_subject_message
Tests to see if the if an email that matches the passed subject and message is in the inbox
It will also return the found message
It takes three params
  1) a valid Net::IMAP::Simple object
  2) a Scalar value for subject
  2) a Scalar vlaue for message
Usage
  $self->get_email_with_message_ok($imap,"bob you there",'Never got your email');
  __END__
