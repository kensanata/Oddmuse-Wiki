# Copyright (C) 2004, 2005, 2009  Alex Schroeder <alex@gnu.org>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('anchors.pl', 'Local Anchor Extension', undef, '2.3.4-18-g66972c4');

push(@MyRules, \&AnchorsRule);

sub AnchorsRule {
  if (m/\G\[\[\#$FreeLinkPattern\]\]/gc) {
    return $q->a({-href=>'#' . FreeToNormal($1), -class=>'local anchor'}, $1);
  } elsif ($BracketWiki && m/\G\[\[\#$FreeLinkPattern\|([^\]]+)\]\]/gc) {
    return $q->a({-href=>'#' . FreeToNormal($1), -class=>'local anchor'}, $2);
  } elsif ($BracketWiki && m/\G(\[\[$FreeLinkPattern\#$FreeLinkPattern\|([^\]]+)\]\])/cog
	   or m/\G(\[\[\[$FreeLinkPattern\#$FreeLinkPattern\]\]\])/cog
	   or m/\G(\[\[$FreeLinkPattern\#$FreeLinkPattern\]\])/cog) {
    # This one is not a dirty rule because the output is always a page
    # link, never an edit link (unlike normal free links).
    my $bracket = (substr($1, 0, 3) eq '[[[');
    my $id = $2 . '#' . $3;
    my $text = $4;
    my $class = 'local anchor';
    my $title = '';
    $id = FreeToNormal($id);
    if (!$text && $bracket) {
      $text = BracketLink(++$FootnoteNumber); # s/_/ /g happens further down!
      $class .= ' number';
      $title = $id; # override title
      $title =~ s/_/ /g if $free;
    }
    $text = $id unless $text;
    $text =~ s/_/ /g;
    return ScriptLink(UrlEncode($id), $text, $class, undef, $title);
  } elsif (m/\G\[\:$FreeLinkPattern\]/gc) {
    return $q->a({-name=>FreeToNormal($1), -class=>'anchor'}, '');
  }
  return;
}
# Copyright (C) 2013  Alex Schroeder <alex@gnu.org>

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

=head1 Ban Contributors Extension

This module adds "Ban contributors" to the administration page. If you
click on it, it will list all the recent contributors to the page
you've been looking at. Each contributor (IP or hostname) will be
compared to the list of regular expressions on the C<BannedHosts> page
(see C<$BannedHosts>). If the contributor is already banned, this is
mentioned. If the contributor is not banned, you'll see a button
allowing you to ban him or her immediately. If you click the button,
the IP or hostname will be added to the C<BannedHosts> page for you.

=cut

AddModuleDescription('ban-contributors.pl', 'Ban Contributors Extension', undef, '2.3.4-18-g66972c4');

push(@MyAdminCode, \&BanMenu);

sub BanMenu {
  my ($id, $menuref, $restref) = @_;
  if ($id and UserIsAdmin()) {
    push(@$menuref, ScriptLink('action=ban;id=' . UrlEncode($id),
			       T('Ban contributors')));
  }
}

$Action{ban} = \&DoBanHosts;

sub IsItBanned {
  my ($it, $regexps) = @_;
  my $re = undef;
  foreach my $regexp (@$regexps) {
    eval { $re = qr/$regexp/i; };
    if (defined($re) && $it =~ $re) {
      return $it;
    }
  }
}

sub DoBanHosts {
  my $id = shift;
  my $content = GetParam('content', '');
  my $host = GetParam('host', '');
  if ($content) {
    SetParam('text', GetPageContent($BannedContent)
	     . $content . " # " . CalcDay($Now) . " "
	     . NormalToFree($id) . "\n");
    SetParam('summary', NormalToFree($id));
    DoPost($BannedContent);
  } elsif ($host) {
    $host =~ s/\./\\./g;
    SetParam('text', GetPageContent($BannedHosts)
	     . "^" . $host . " # " . CalcDay($Now) . " "
	     . NormalToFree($id) . "\n");
    SetParam('summary', NormalToFree($id));
    DoPost($BannedHosts);
  } else {
    ValidIdOrDie($id);
    print GetHeader('', Ts('Ban Contributors to %s', NormalToFree($id)));
    SetParam('rcidonly', $id);
    SetParam('all', 1);
    SetParam('showedit', 1);
    my %contrib = ();
    for my $line (GetRcLines()) {
      $contrib{$line->[4]}->{$line->[5]} = 1 if $line->[4];
    }
    my @regexps = ();
    foreach (split(/\n/, GetPageContent($BannedHosts))) {
      if (/^\s*([^#]\S+)/) { # all lines except empty lines and comments, trim whitespace
	push(@regexps, $1);
      }
    }
    print '<div class="content ban">';
    foreach (sort(keys %contrib)) {
      my $name = $_;
      delete $contrib{$_}{''};
      $name .= " (" . join(", ", sort(keys(%{$contrib{$_}}))) . ")";
      if (IsItBanned($_, \@regexps)) {
	print $q->p(Ts("%s is banned", $name));
      } else {
	print GetFormStart(undef, 'get', 'ban'),
	  GetHiddenValue('action', 'ban'),
	  GetHiddenValue('id', $id),
	  GetHiddenValue('host', $_),
	  GetHiddenValue('recent_edit', 'on'),
	  $q->p($name, $q->submit(T('Ban!'))), $q->end_form();
      }
    }
  }
  PrintFooter();
}

=head2 Rollback

If you are an admin and rolled back a single page, this extension will
list the URLs your rollback removed (assuming that those URLs are part
of the spam) and it will allow you to provide a regular expression
that will be added to BannedHosts.

=cut

*OldBanContributorsWriteRcLog = *WriteRcLog;
*WriteRcLog = *NewBanContributorsWriteRcLog;

sub NewBanContributorsWriteRcLog {
  my ($tag, $id, $to) = @_;
  if ($tag eq '[[rollback]]' and $id and $to > 0
      and $OpenPageName eq $id and UserIsAdmin()) {
    # we currently have the clean page loaded, so we need to reload
    # the spammed revision (there is a possible race condition here)
    my ($old) = GetTextRevision($Page{revision}-1, 1);
    my %urls = map {$_ => 1 } $old =~ /$UrlPattern/og;
    # we open the file again to force a load of the despammed page
    foreach my $url ($Page{text} =~ /$UrlPattern/og) {
      delete($urls{$url});
    }
    # we also remove any candidates that are already banned
    my @regexps = ();
    foreach (split(/\n/, GetPageContent($BannedContent))) {
      if (/^\s*([^#]\S+)/) { # all lines except empty lines and comments, trim whitespace
	push(@regexps, $1);
      }
    }
    foreach my $url (keys %urls) {
      delete($urls{$url}) if IsItBanned($url, \@regexps);
    }
    if (keys %urls) {
      print $q->p(Ts("These URLs were rolled back. Perhaps you want to add a regular expression to %s?",
		     GetPageLink($BannedContent)));
      print $q->pre(join("\n", sort keys %urls));
      print GetFormStart(undef, 'get', 'ban'),
	    GetHiddenValue('action', 'ban'),
	    GetHiddenValue('id', $id),
	    GetHiddenValue('recent_edit', 'on'),
	    $q->p($q->label({-for=>'content'}, T('Regular expression:')), " ",
		  $q->textfield(-name=>'content', -size=>30), " ",
		  $q->submit(T('Ban!'))),
	    $q->end_form();
    };
    print $q->p(T("Consider banning the IP number as well: "),
		ScriptLink('action=ban;id=' . UrlEncode($id), T('Ban contributors')));
  };
  return OldBanContributorsWriteRcLog(@_);
}
# Copyright (C) 2012  Alex Schroeder <alex@gnu.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('fix-encoding.pl', 'Fix Encoding', undef, '2.3.4-18-g66972c4');

$Action{'fix-encoding'} = \&FixEncoding;

sub FixEncoding {
  my $id = shift;
  ValidIdOrDie($id);
  RequestLockOrError();
  OpenPage($id);
  my $text = $Page{text};
  utf8::decode($text);
  Save($id, $text, T('Fix character encoding'), 1) if $text ne $Page{text};
  ReleaseLock();
  ReBrowsePage($id);
}

$Action{'fix-escaping'} = \&FixEscaping;

sub FixEscaping {
  my $id = shift;
  ValidIdOrDie($id);
  RequestLockOrError();
  OpenPage($id);
  my $text = UnquoteHtml($Page{text});
  Save($id, $text, T('Fix HTML escapes'), 1) if $text ne $Page{text};
  ReleaseLock();
  ReBrowsePage($id);
}

push(@MyAdminCode, \&FixEncodingMenu);

sub FixEncodingMenu {
  my ($id, $menuref, $restref) = @_;
  if ($id && GetParam('username')) {
    push(@$menuref,
	 ScriptLink('action=fix-encoding;id=' . UrlEncode($id),
		    T('Fix character encoding')));
    push(@$menuref,
	 ScriptLink('action=fix-escaping;id=' . UrlEncode($id),
		    T('Fix HTML escapes')));
  }
}
#!/usr/bin/env perl
# ====================[ forms.pl                           ]====================

AddModuleDescription('forms.pl', 'Form Extension', undef, '2.3.4-18-g66972c4');

# ....................{ MARKUP                             }....................
push(@MyRules, \&FormsRule);

sub FormsRule {
  if (-f GetLockedPageFile($OpenPageName) or (InElement('div', '^class="crossbar"$') and
      -f GetLockedPageFile($CrossbarPageName))) {
    if (/\G(\&lt;form.*?\&lt;\/form\&gt;)/cgs) {
      my $form = $1;
      my $oldpos = pos;
      Clean(CloseHtmlEnvironments());
      Dirty($form);
      $form =~ s/\%([a-z]+)\%/GetParam($1)/ge;
      $form =~ s/\$([a-z]+)\$/$q->span({-class=>'param'}, GetParam($1))
        .$q->input({-type=>'hidden', -name=>$1, -value=>GetParam($1)})/ge;
      print UnquoteHtml($form);
      pos = $oldpos;
      return AddHtmlEnvironment('p');
    }
    elsif (m/\G\&lt;html\&gt;(.*?)\&lt;\/html\&gt;/cgs) {
      return UnquoteHtml($1);
    }
  }
  return;
}

=head1 COPYRIGHT AND LICENSE

The information below applies to everything in this distribution,
except where noted.

Copyleft  2008 by B.w.Curry <http://www.raiazome.com>.
Copyright 2004 by Alex Schroeder <alex@emacswiki.org>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see L<http://www.gnu.org/licenses/>.

=cut
# Copyright (C) 2004, 2005, 2006, 2007  Alex Schroeder <alex@emacswiki.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
#    Free Software Foundation, Inc.
#    59 Temple Place, Suite 330
#    Boston, MA 02111-1307 USA

AddModuleDescription('image.pl', 'Image Extension', undef, '2.3.4-18-g66972c4');

use vars qw($ImageUrlPath);

$ImageUrlPath = '/images';      # URL where the images are to be found

push(@MyRules, \&ImageSupportRule);

# [[image/class:page name|alt text|target]]

sub ImageSupportRule {
  my $result = undef;
  if (m!\G\[\[image((/[a-z]+)*)( external)?:\s*([^]|]+?)\s*(\|[^]|]+?)?\s*(\|[^]|]*?)?\s*(\|[^]|]*?)?\s*(\|[^]|]*?)?\s*\]\](\{([^}]+)\})?!gc) {
    my $oldpos = pos;
    my $class = 'image' . $1;
    my $external = $3;
    my $name = $4;
    # Don't generate an alt text if none was specified, since the rule
    # forces you to pick an alt text if you're going to provide a
    # link target.
    my $alt = UnquoteHtml($5 ? substr($5, 1) : '');
    $alt = NormalToFree($name)
      if not $alt and not $external and $name !~ /^$FullUrlPattern$/;
    my $link = $6 ? substr($6, 1) : '';
    my $caption = $7 ? substr($7, 1) : '';
    my $reference = $8 ? substr($8, 1) : '';
    my $comments = $10;
    my $id = FreeToNormal($name);
    $class =~ s!/! !g;
    my $linkclass = $class;
    my $found = 1;
    # link to the image if no link was given
    $link = $name unless $link;
    if ($link =~ /^($FullUrlPattern|$FreeInterLinkPattern)$/
	or $link =~ /^$FreeLinkPattern$/ and not $external) {
      ($link, $linkclass) = ImageGetExternalUrl($link, $linkclass);
    } else {
      $link = $ImageUrlPath . '/' . ImageUrlEncode($link);
    }
    my $src = $name;
    if ($src =~ /^($FullUrlPattern|$FreeInterLinkPattern)$/) {
      ($src) = ImageGetExternalUrl($src);
    } elsif ($src =~ /^$FreeLinkPattern$/ and not $external) {
      $found = $IndexHash{FreeToNormal($src)};
      $src = ImageGetInternalUrl($src) if $found;
    } else {
      $src = $ImageUrlPath . '/' . ImageUrlEncode($name);
    }
    if ($found) {
      $result = $q->img({-src=>$src, -alt=>$alt, -title=>$alt, -class=>'upload'});
      $result = $q->a({-href=>$link, -class=>$linkclass}, $result);
      if ($comments) {
	for (split '\n', $comments) {
	  my $valRegex = qr/(([0-9.]+[a-z]*%?)\s+)/;
	  if ($_ =~ /^\s*(([a-zA-Z ]+)\/)?$valRegex$valRegex$valRegex$valRegex(.*)$/) { # can't use {4} here? :(
	    my $commentClass = $2 ? "imagecomment $2" : 'imagecomment';
	    $result .= $q->div({-class=>$commentClass, -style=>"position: absolute; top: $6; left: $4; width: $8; height: $10"}, $11);
	  }
	}
	$result = CloseHtmlEnvironments() . $q->div({-class=>"imageholder", -style=>"position: relative"}, $result);
      }
    } else {
      $result = GetDownloadLink($src, 1, undef, $alt);
    }
    if ($caption) {
      if ($reference) {
	my $refclass = $class;
	($reference, $refclass) = ImageGetExternalUrl($reference, $refclass);
	$caption = $q->a({-href=>$reference, -class=>$refclass}, $caption);
      }
      $result .= $q->br() . $q->span({-class=>'caption'}, $caption);
      $result = CloseHtmlEnvironments() . $q->div({-class=>$class}, $result);
    }
    pos = $oldpos;
  }
  return $result;
}

sub ImageUrlEncode {
  # url encode everything except for slashes
  return join('/', map { UrlEncode($_) } split(/\//, shift));
}

sub ImageGetExternalUrl {
  my ($link, $class) = @_;
  if ($link =~ /^$FullUrlPattern$/) {
    $link = UnquoteHtml($link);
    $class .= ' outside';
  } elsif ($link =~ /^$FreeInterLinkPattern$/) {
    my ($site, $page) = split(/:/, $link, 2);
    $link = GetInterSiteUrl($site, $page, 1); # quote!
    $class .= ' inter ' . $site;
  } else {
    $link = FreeToNormal($link);
    if (substr($link, 0, 1) eq '/') {
      # do nothing -- relative URL on the same server
    } elsif ($UsePathInfo and !$Monolithic) {
      $link = $ScriptName . '/' . $link;
    } elsif ($Monolithic) {
      $link = '#' . $link;
    } else {
      $link = $ScriptName . '?' . $link;
    }
  }
  return ($link, $class);
}

# split off to support overriding from Static Extension
sub ImageGetInternalUrl {
  my $id = FreeToNormal(shift);
  if ($UsePathInfo) {
    return $ScriptName . "/download/" . UrlEncode($id);
  }
  return $ScriptName . "?action=download;id=" . UrlEncode($id);
}
# Copyright (C) 2004–2014  Alex Schroeder <alex@gnu.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('journal-rss.pl', 'Journal RSS Extension', undef, '2.3.4-18-g66972c4');

$Action{journal} = \&DoJournalRss;

# Currently RSS works like RecentChanges, which is not what bloggers
# expect.  Produce a RSS feed that mimicks exactly how the journal tag
# works.

sub DoJournalRss {
  return if $CollectingJournal; # avoid infinite loops
  local $CollectingJournal = 1;
  # Fake the result of GetRcLines()
  local *GetRcLines = *JournalRssGetRcLines;
  print GetHttpHeader('application/xml') . GetRcRss();
}

sub JournalRssGetRcLines {
  my $num = GetParam('rsslimit', 10);
  my $match = GetParam('match', '^\d\d\d\d-\d\d-\d\d');
  my $search = GetParam('search', '');
  my $reverse = GetParam('reverse', 0);
  my $monthly = GetParam('monthly', 0);
  my @pages = sort JournalSort (grep(/$match/, $search ? SearchTitleAndBody($search) : AllPagesList()));
  if ($monthly and not $match) {
    my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday) = gmtime();
    $match = '^' . sprintf("%04d-%02d", $year+1900, $mon+1) . '-\d\d';
  }
  if ($reverse) {
    @pages = reverse @pages;
  }
  # FIXME: Missing 'future' and 'past' keywords.
  # FIXME: Do we need 'offset'? I don't think so.
  my @result = ();
  foreach my $id (@pages) {
    # Now save information required for saving the cache of the current page.
    local %Page;
    local $OpenPageName = '';
    OpenPage($id);
    # If this is a minor edit, ignore it. Load the last major revision
    # instead, if you can.
    if ($Page{minor}) {
      # Perhaps the old kept revision is gone due to $KeepMajor=0 or
      # admin.pl or because a page was created as a minor change and
      # never edited. Reading kept revisions in this case results in
      # an error.
      eval {
 	%Page = GetKeptRevision($Page{lastmajor});
      };
      next if $@;
    }
    next if $Page{text} =~ /^\s*$/; # only whitespace is also to be deleted
    next if $DeletedPage && substr($Page{text}, 0, length($DeletedPage))
      eq $DeletedPage; # no regexp
    # Generate artifical rows in the list to pass to GetRcRss. We need
    # to open every single page, because the meta-data ordinarily
    # available in the rc.log file is not available to us. This is why
    # we observe the rsslimit parameter. Without it, we would have to
    # open *all* date pages.
    my @languages = split(/,/, $languages);
    push (@result, [$Page{ts}, $id, $Page{minor}, $Page{summary}, $Page{host},
		    $Page{username}, $Page{revision}, \@languages,
		    GetCluster($Page{text})]);
    last if $num ne 'all' and $#result + 1 >= $num;
  }
  return @result;
}

# Prevent near links from being printed as a result of the search.
push(@MyInitVariables, sub {
       $NearLinksException{journal} = 1;
     });
# Copyright (C) 2004  Alex Schroeder <alex@emacswiki.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
#    Free Software Foundation, Inc.
#    59 Temple Place, Suite 330
#    Boston, MA 02111-1307 USA

AddModuleDescription('links.pl', 'Link Data Extension', undef, '2.3.4-18-g66972c4');

$Action{links} = \&DoLinks;

sub DoLinks {
  my @args = (GetParam('raw', 0), GetParam('url', 0), GetParam('inter', 0), GetParam('links', 1));
  if (GetParam('raw', 0)) {
    print GetHttpHeader('text/plain');
    PrintLinkList(GetFullLinkList(@args));
  } else {
    print GetHeader('', QuoteHtml(T('Full Link List')), '');
    PrintLinkList(GetFullLinkList(@args));
    PrintFooter();
  }
}

sub PrintLinkList {
  my %links = %{(shift)};
  my $existingonly = GetParam('exists', 0);
  if (GetParam('raw', 0)) {
    foreach my $page (sort keys %links) {
      foreach my $link (@{$links{$page}}) {
	print "\"$page\" -> \"$link\"\n" if not $existingonly or $IndexHash{$link};
      }
    }
  } else {
    foreach my $page (sort keys %links) {
      print $q->p(GetPageLink($page) . ': ' . join(' ', @{$links{$page}}));
    }
  }
}

sub GetFullLinkList { # opens all pages!
  my ($raw, $url, $inter, $link) = @_;
  my @pglist = AllPagesList();
  my %result;
  InterInit();
  foreach my $name (@pglist) {
    OpenPage($name);
    my @links = GetLinkList($raw, $url, $inter, $link);
    @{$result{$name}} = @links if @links;
  }
  return \%result;
}

sub GetLinkList { # for the currently open page
  my ($raw, $url, $inter, $link) = @_;
  my @blocks = split($FS, $Page{blocks});
  my @flags = split($FS, $Page{flags});
  my %links;
  foreach my $block (@blocks) {
    if (shift(@flags)) {  # dirty block and interlinks or normal links
      if ($inter and ($BracketText && $block =~ m/^(\[$InterLinkPattern\s+([^\]]+?)\])$/o
		      or $BracketText && $block =~ m/^(\[\[$FreeInterLinkPattern\|([^\]]+?)\]\])$/o
		      or $block =~ m/^(\[$InterLinkPattern\])$/o
		      or $block =~ m/^(\[\[\[$FreeInterLinkPattern\]\]\])$/o
		      or $block =~ m/^($InterLinkPattern)$/o
		      or $block =~ m/^(\[\[$FreeInterLinkPattern\]\])$/o)) {
	$links{$raw ? $2 : GetInterLink($2, $3)} = 1 if $InterSite{substr($2,0,index($2, ':'))};
      } elsif ($link
	       and (($WikiLinks and $block !~ m/!$LinkPattern/o
		     and ($BracketWiki && $block =~ m/^(\[$LinkPattern\s+([^\]]+?)\])$/o
			  or $block =~ m/^(\[$LinkPattern\])$/o
			  or $block =~ m/^($LinkPattern)$/o))
		    or ($FreeLinks
			and ($BracketWiki && $block =~ m/^(\[\[$FreeLinkPattern\|([^\]]+)\]\])$/o
			     or $block =~ m/^(\[\[\[$FreeLinkPattern\]\]\])$/o
			     or $block =~ m/^(\[\[$FreeLinkPattern\]\])$/o)))) {
	$links{$raw ? FreeToNormal($2) : GetPageOrEditLink($2, $3)} = 1;
      } elsif ($url and $block =~ m/^\[$FullUrlPattern\]$/og) {
	$links{$raw ? $1 : GetUrl($1)} = 1;
      }
    } elsif ($url) {		# clean block and url
      while ($block =~ m/$UrlPattern/og) {
	$links{$raw ? $1 : GetUrl($1)} = 1;
      }
      while ($block =~ m/\[$FullUrlPattern\s+[^\]]+?\]/og) {
	$links{$raw ? $1 : GetUrl($1)} = 1;
      }
    }
  }
  return sort keys %links;
}
# Copyright (C) 2006, 2008, 2009  Alex Schroeder <alex@gnu.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('load-lang.pl', 'Language Browser Preferences', undef, '2.3.4-18-g66972c4');

$CookieParameters{interface} = '';

use vars qw($CurrentLanguage $LoadLanguageDir);

my %library= ('bg' => 'bulgarian-utf8.pl',
	      'de' => 'german-utf8.pl',
	      'es' => 'spanish-utf8.pl',
	      'fr' => 'french-utf8.pl',
	      'fi' => 'finnish-utf8.pl',
	      'gr' => 'greek-utf8.pl',
	      'he' => 'hebrew-utf8.pl',
	      'it' => 'italian-utf8.pl',
	      'ja' => 'japanese-utf8.pl',
	      'ko' => 'korean-utf8.pl',
	      'nl' => 'dutch-utf8.pl',
	      'pl' => 'polish-utf8.pl',
	      'pt' => 'portuguese-utf8.pl',
	      'ro' => 'romanian-utf8.pl',
	      'ru' => 'russian-utf8.pl',
	      'se' => 'swedish-utf8.pl',
	      'sr' => 'serbian-utf8.pl',
	      'zh' => 'chinese-utf8.pl',
	      'zh-cn' => 'chinese_cn-utf8.pl',
	      'zh-tw' => 'chinese-utf8.pl',
	     );

sub LoadLanguage {
  # my $requested_language = "da, en-gb;q=0.8, en;q=0.7";
  my $requested_language = $q->http('Accept-language');
  my @languages = split(/ *, */, $requested_language);
  my %Lang = ();
  foreach $_ (@languages) {
    my $qual = 1;
    $qual = $1 if (/q=([0-9.]+)/);
    $Lang{$qual} = $1 if (/^([-a-z]+)/);
  }
  my $lang = GetParam('interface', '');
  $Lang{2} = $lang if $lang;
  my @prefs = sort { $b <=> $a } keys %Lang;
  # print ($q->header . $q->start_html
  #      . $q->pre("input: $requested_language\n"
  #                . "Result: "
  #                . join(', ', map { "$_ ($Lang{$_})" } @prefs))
  #      . $q->end_html) && exit if GetParam('debug', '');
  foreach $_ (@prefs) {
    last if $Lang{$_} eq 'en'; # the default
    my $file = $library{$Lang{$_}};
    $file = "$LoadLanguageDir/$file" if defined $LoadLanguageDir;
    if (-r $file) {
      do $file;
      do "$ConfigFile-$Lang{$_}" if -r "$ConfigFile-$Lang{$_}";
      $CurrentLanguage = $Lang{$_};
      my $f;
      if ($NamespaceCurrent) {
	$f = "$DataDir/../README.$Lang{$_}";
      } else {
	$f = "$DataDir/README.$Lang{$_}";
      }
      $ReadMe = $f if -r $f;
      last;
    }
  }
}

# Must load language dependent config files before running init code for
# gotobar.pl and similar extensions.
unshift(@MyInitVariables, \&LoadLanguage);
# Copyright (C) 2004–2015  Alex Schroeder <alex@gnu.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('markup.pl', 'Markup Extension', undef, '2.3.4-18-g66972c4');

use vars qw(%MarkupPairs %MarkupForcedPairs %MarkupSingles %MarkupLines
	    $MarkupQuotes $MarkupQuoteTable);

$MarkupQuotes = 1;

# $MarkupQuotes	'hi'	"hi"	I'm	Favored in
#	0	'hi'	"hi"	I'm	Typewriters
#	1	‘hi’	“hi”	I’m	Britain and North America
#	2	‹hi›	«hi»	I’m	France and Italy
#	3	›hi‹	»hi«	I’m	Germany
#	4	‚hi’	„hi”	I’m	Germany

#                        0           1           2           3           4
$MarkupQuoteTable = [[  "'",        "'",        '"',        '"'     ,   "'"     ], # 0
		     ['&#x2018;', '&#x2019;', '&#x201d;', '&#x201c;', '&#x2019;'], # 1
		     ['&#x2039;', '&#x203a;', '&#x00bb;', '&#x00ab;', '&#x2019;'], # 2
		     ['&#x203a;', '&#x2039;', '&#x00ab;', '&#x00bb;', '&#x2019;'], # 3
		     ['&#x201a;', '&#x2018;', '&#x201c;', '&#x201e;', '&#x2019;'], # 4
		    ];

# $MarkupQuoteTable->[2]->[0] ‹
# $MarkupQuoteTable->[2]->[1] ›
# $MarkupQuoteTable->[2]->[2] »
# $MarkupQuoteTable->[2]->[3] «
# $MarkupQuoteTable->[2]->[4] ’

push(@MyRules, \&MarkupRule);
# The ---- rule in usemod.pl conflicts with the --- rule
$RuleOrder{\&MarkupRule} = 150;

%MarkupPairs = ('*' => 'b',
		'/' => 'i',
		'_' => ['em', {'style'=>'text-decoration: underline; font-style: normal;'}],
		'~' => 'em',
	       );

%MarkupForcedPairs = ("{{{\n" => ['pre', undef, '}}}'],
		      '##' => 'code',
		      '%%' => 'span',
		      '**' => 'b',
		      '//' => 'i',
		      '__' => ['em', {'style'=>'text-decoration: underline; font-style: normal;'}],
		      '~~' => 'em',
		     );

# This could be done using macros, however: If we convert to the
# numbered entity, the next person editing finds it hard to read.  If
# we convert to a unicode character, it is no longer obvious how to
# achieve it.
%MarkupSingles = ('...' => '&#x2026;', # HORIZONTAL ELLIPSIS
		  '---' => '&#x2014;', # EM DASH
		  '-- ' => '&#x2013; ', # EN DASH
		  '-> ' => '&#x2192;&#x00a0;', # RIGHTWARDS ARROW, NO-BREAK SPACE
		  '<-'  => '&#8592;',
		  '<--' => '&#8592;',
		  '-->' => '&#x2192;',
		  '=>'  => '&#8658;',
		  '==>' => '&#8658;',
		  '<=>' => '&#8660;',
		  '+/-' => '&#x00b1;',
		 );

%MarkupLines = ('>' => 'pre',
	       );

# either a single letter, or a string that begins with a single letter and ends with a non-space
my $words = '([A-Za-z\x{0080}-\x{fffd}](?:[-%.,:;\'"!?0-9 A-Za-z\x{0080}-\x{fffd}]*?[-%.,:;\'"!?0-9A-Za-z\x{0080}-\x{fffd}])?)';
# zero-width assertion to prevent km/h from counting
my $nowordstart = '(?:(?<=[^-0-9A-Za-z\x{0080}-\x{fffd}])|^)';
# zero-width look-ahead assertion to prevent km/h from counting
my $nowordend = '(?=[^-0-9A-Za-z\x{0080}-\x{fffd}]|$)';

my $markup_pairs_re = '';
my $markup_forced_pairs_re = '';
my $markup_singles_re = '';
my $markup_lines_re = '';

# do not add all block elements, because not all of them make sense,
# as they cannot be nested -- thus it would not be possible to put
# list items inside a list element, for example.
my %block_element = map { $_ => 1 } qw(p blockquote address div h1 h2
				       h3 h4 h5 h6 pre);

# do this later so that the user can customize the vars
push(@MyInitVariables, \&MarkupInit);

sub MarkupInit {
  $markup_pairs_re = '\G([' . join('', (map { quotemeta(QuoteHtml($_)) }
					keys(%MarkupPairs))) . '])';
  $markup_pairs_re = qr/${nowordstart}${markup_pairs_re}${words}\1${nowordend}/;
  $markup_forced_pairs_re = '\G(' . join('|', (map { quotemeta(QuoteHtml($_)) }
					       keys(%MarkupForcedPairs))) . ')';
  $markup_forced_pairs_re = qr/$markup_forced_pairs_re/;
  $markup_singles_re = '\G(' . join('|', (map { quotemeta(QuoteHtml($_)) }
					  sort {$b cmp $a} # longer regex first
					  keys(%MarkupSingles))) . ')';
  $markup_singles_re = qr/$markup_singles_re/;
  $markup_lines_re = '\G(' . join('|', (map { quotemeta(QuoteHtml($_)) }
					keys(%MarkupLines))) . ')(.*\n?)';
  $markup_lines_re = qr/$markup_lines_re/;
}

sub MarkupTag {
  my ($tag, $str) = @_;
  my ($start, $end);
  if (ref($tag)) {
    my $arrayref = $tag;
    my ($tag, $hashref) = @{$arrayref};
    my %hash = %{$hashref};
    $start = $end = $tag;
    foreach my $attr (keys %hash) {
      $start .= ' ' . $attr . '="' . $hash{$attr} . '"';
    }
  } else {
    $start = $end = $tag;
  }
  my $result = "<$start>$str</$end>";
  $result = CloseHtmlEnvironments() . $result . AddHtmlEnvironment('p')
    if $block_element{$start};
  return $result;
}

sub MarkupRule {
  if ($bol and %MarkupLines and m/$markup_lines_re/gc) {
    my ($tag, $str) = ($1, $2);
    $str = $q->span($tag) . $str;
    while (m/$markup_lines_re/gc) {
      $str .= $q->span($1) . $2;
    }
    return CloseHtmlEnvironments()
      . MarkupTag($MarkupLines{UnquoteHtml($tag)}, $str)
      . AddHtmlEnvironment('p');
  } elsif (%MarkupSingles and m/$markup_singles_re/gc) {
    return $MarkupSingles{UnquoteHtml($1)};
  } elsif (%MarkupForcedPairs and m/$markup_forced_pairs_re/gc) {
    my $tag = $1;
    my $start = $tag;
    my $end = $tag;
    # handle different end tag
    my $data = $MarkupForcedPairs{UnquoteHtml($tag)};
    if (ref($data)) {
      my @data = @{$data};
      $start = $data[0] if $data[0];
      $end = $data[2] if $data[2];
    }
    my $endre = quotemeta($end);
    $endre .= '[ \t]*\n?' if $block_element{$start}; # skip trailing whitespace if block
    # may match the empty string, or multiple lines, but may not span
    # paragraphs.
    if ($endre and m/\G$endre/gc) {
      return $tag . $end;
    } elsif ($tag eq $end && m/\G((:?.+?\n)*?.+?)$endre/gc) { # may not span paragraphs
      return MarkupTag($data, $1);
    } elsif ($tag ne $end && m/\G((:?.|\n)+?)$endre/gc) {
      return MarkupTag($data, $1);
    } else {
      return $tag;
    }
  } elsif (%MarkupPairs and m/$markup_pairs_re/gc) {
    return MarkupTag($MarkupPairs{UnquoteHtml($1)}, $2);
  } elsif ($MarkupPairs{'/'} and m|\G~/|gc) {
    return '~/'; # fix ~/elisp/ example
  } elsif ($MarkupPairs{'/'} and m|\G(/[-A-Za-z0-9\x{0080}-\x{fffd}/]+/$words/)|gc) {
    return $1; # fix /usr/share/lib/! example
  }
  # "foo
  elsif ($MarkupQuotes and (m/\G(?<=[[:space:]])"/cg
			    or pos == 0 and m/\G"/cg)) {
    return $MarkupQuoteTable->[$MarkupQuotes]->[3];
  }
  # foo"
  elsif ($MarkupQuotes and (m/\G"(?=[[:space:][:punct:]])/cg
			      or m/\G"\z/cg)) {
    return $MarkupQuoteTable->[$MarkupQuotes]->[2];
  }
  # foo."
  elsif ($MarkupQuotes and (m/\G(?<=[[:punct:]])"/cg)) {
    return $MarkupQuoteTable->[$MarkupQuotes]->[3];
  }
  # single quotes at the beginning of the buffer
  elsif ($MarkupQuotes and pos == 0 and m/\G'/cg) {
    return $MarkupQuoteTable->[$MarkupQuotes]->[0];
  }
  # 'foo
  elsif ($MarkupQuotes and (m/\G(?<=[[:space:]])'/cg
			    or pos == 0 and m/\G'/cg)) {
    return $MarkupQuoteTable->[$MarkupQuotes]->[0];
  }
  # foo'
  elsif ($MarkupQuotes and (m/\G'(?=[[:space:][:punct:]])/cg
			    or m/\G'\z/cg)) {
    return $MarkupQuoteTable->[$MarkupQuotes]->[1];
  }
  # foo's
  elsif ($MarkupQuotes and m/\G(?<![[:space:]])'(?![[:space:][:punct:]])/cg) {
    return $MarkupQuoteTable->[$MarkupQuotes]->[4];
  }
  return;
}
# Copyright (C) 2003, 2004, 2005, 2006, 2007  Alex Schroeder <alex@gnu.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('permanent-anchors.pl', 'Permanent Anchors', undef, '2.3.4-18-g66972c4');

=head1 Permanent Anchors

This module allows you to create link targets within a page. These
link targets are called named anchors in HTML. The anchors provided by
this module are permanent, because moving the anchor from one page to
another does not affect the links pointing to it. You link to these
named anchors as if they were pagenames. For users, it makes no
difference.

=cut

use vars qw(%PermanentAnchors %PagePermanentAnchors $PermanentAnchorsFile);

$PermanentAnchorsFile = "$DataDir/permanentanchors";

=head2 Definition

Permanent anchors are defined by using square brackets and a double
colon, like this: C<[::Example]>.

If you define a permanent anchor that already exists, the new
definition will have no effect. Instead you will be shown a link to
the existing permanent anchor so that you can easily resolve the
conflict.

If you define a permanent anchor and a page of the same name already
exists, the definition will work, and all links will point to the
permanent anchor. You will also be given a link to the existing page
so that you can easily resolve the conflict (eg. by deleting the
page). Note that if you mark the page for deletion, you will still
have to wait for page expiry to kick in and actually delete the page
before the message disappears.

During anchor definition a lock is created in the temporary directory.
If Oddmuse encounters a lock while defining a permanent anchor, it
will wait a few seconds and try again. If the lock cannot be obtained,
the definition fails. The unlock action available from the
administration page allows you to remove any stale locks once you're
sure the locks have been left behind by a crash. After having removed
the stale lock, edit the page with the permanent anchor definition
again.

When linking to a permanent anchor on the same page, you'll notice
that this only works flawlessly if the definition comes first. When
rendering a page, permanent anchor definitions and links are parsed in
order. Thus, if the link comes first, the permanent anchor definition
is not yet available. Once you invalidate the HTML cache (by editing
another page or by removing the C<pageidx> file from the data
directory), this situation will have fixed itself.

=cut

push(@MyRules, \&PermanentAnchorsRule);

sub PermanentAnchorsRule {
  my ($locallinks, $withanchors) = @_;
  if (m/\G(\[::$FreeLinkPattern\])/cog) {
    #[::Free Link] permanent anchor create only $withanchors
    Dirty($1);
    if ($withanchors) {
      print GetPermanentAnchor($2);
    } else {
      print $q->span({-class=>'permanentanchor'}, $2);
    }
  }
  return;
}

sub GetPermanentAnchor {
  my $id = FreeToNormal(shift);
  my $text = NormalToFree($id);
  my ($class, $resolved, $title, $exists) = ResolveId($id);
  if ($class eq 'alias' and $title ne $OpenPageName) {
    return '[' . Ts('anchor first defined here: %s',
		    ScriptLink(UrlEncode($resolved), $text, 'alias')) . ']';
  } elsif ($PermanentAnchors{$id} ne $OpenPageName
	   # 10 tries, 3 second wait, die on error
	   and RequestLockDir('permanentanchors', 10, 3, 1)) {
    # Somebody may have added a permanent anchor in the mean time.
    # Comparing $LastUpdate to the $IndexFile mtime does not work for
    # subsecond changes and updates are rare, so just reread the file!
    PermanentAnchorsInit();
    $PermanentAnchors{$id} = $OpenPageName;
    WritePermanentAnchors();
    ReleaseLockDir('permanentanchors');
  }
  $PagePermanentAnchors{$id} = 1; # add to the list of anchors in page
  my $html = GetSearchLink($id, 'definition', $id,
    T('Click to search for references to this permanent anchor'));
  $html .= ' [' . Ts('the page %s also exists',
		     ScriptLink("action=browse;anchor=0;id="
				. UrlEncode($id), NormalToFree($id), 'local'))
    . ']' if $exists;
  return $html;
}

=head2 Storage

Permanent anchor definitions need to be stored in a separate file.
Otherwise linking to a permanent anchor would require a search of the
entire page database. The permanent anchors are stored in a file
called C<permanentanchors> in the data directory. The location can be
changed by setting C<$PermanentAnchorsFile>.

The format of the file is simple: permanent anchor names and the name
of the page they are defined on follow each other, separated by
whitespace. Spaces within permanent anchor names and page names are
replaced with underlines, as always. Thus, the keys of
C<%PermanentAnchors> is the name of the permanent anchor, and
C<$PermanentAnchors{$name}> is the name of the page it is defined on.

=cut

push(@MyInitVariables, \&PermanentAnchorsInit);

sub PermanentAnchorsInit {
  %PagePermanentAnchors = %PermanentAnchors = ();
  my ($status, $data) = ReadFile($PermanentAnchorsFile);
  return unless $status; # not fatal
  # $FS was used in 1.417 and earlier!
  %PermanentAnchors = split(/\n| |$FS/,$data);
}

sub WritePermanentAnchors {
  my $data = '';
  foreach my $name (keys %PermanentAnchors) {
    $data .= $name . ' ' . $PermanentAnchors{$name} ."\n";
  }
  WriteStringToFile($PermanentAnchorsFile, $data);
}

=head2 Deleting Anchors

When deleting a page Oddmuse needs to delete the corresponding
permanent anchors from its file. This is why the
C<DeletePermanentAnchors> function is called from C<DeletePage>.

When a page is edited, we want to make sure that Oddmuse deletes the
permanent anchors no longer needed from its file. The safest way to do
this is to delete all permanent anchors defined on the page being
edited and redefine them when it is rendered for the first time. This
is achieved by calling C<DeletePermanentAnchors> from C<Save>. After
hitting the save button, the user is automatically redirected to the
new page. This will render the page, and redefine all permanent
anchors.

=cut

*OldPermanentAnchorsDeletePage = *DeletePage;
*DeletePage = *NewPermanentAnchorsDeletePage;

sub NewPermanentAnchorsDeletePage {
  my $status = OldPermanentAnchorsDeletePage(@_);
  return $status if $status; # this would be the error message
  DeletePermanentAnchors(@_); # the only parameter is $id
}

*OldPermanentAnchorsSave = *Save;
*Save = *NewPermanentAnchorsSave;

sub NewPermanentAnchorsSave {
  OldPermanentAnchorsSave(@_);
  DeletePermanentAnchors(@_); # the first parameter is $id
}

sub DeletePermanentAnchors {
  my $id = shift;
  # 10 tries, 3 second wait, die on error
  RequestLockDir('permanentanchors', 10, 3, 1);
  foreach (keys %PermanentAnchors) {
    if ($PermanentAnchors{$_} eq $id and !$PagePermanentAnchors{$_}) {
      delete($PermanentAnchors{$_}) ;
    }
  }
  WritePermanentAnchors();
  ReleaseLockDir('permanentanchors');
}

=head2 Name Resolution

Name resolution is done by C<ResolveId>. This function returns a list
of several items: The CSS class to use, the resolved id, the title
(eg. for popups), and a boolean saying whether the page actually
exists or not. When resolving a permanent anchor, the CSS class used
will be “alias”, the resolved id will be the C<pagename#anchorname>,
the title will be the page name.

You can override this behaviour by providing the parameter
C<anchor=0>. This is used for the link in the warning message “the
page foo also exists.”

=cut

*OldPermanentAnchorsResolveId = *ResolveId;
*ResolveId = *NewPermanentAnchorsResolveId;

sub NewPermanentAnchorsResolveId {
  my $id = shift;
  my $page = $PermanentAnchors{$id};
  if (GetParam('anchor', 1) and $page and $page ne $id) {
    return ('alias', $page . '#' . $id, $page, $IndexHash{$id})
  } else {
    return OldPermanentAnchorsResolveId($id, @_);
  }
}

=head2 Anchor Objects

An anchor object is the text that starts after the anchor definition
and goes up to the next heading, horizontal line, or the end of the
page. By redefining C<GetPageContent> to work on anchor objects we
automatically allow internal transclusion.

=cut

*OldPermanentAnchorsGetPageContent = *GetPageContent;
*GetPageContent = *NewPermanentAnchorsGetPageContent;

sub NewPermanentAnchorsGetPageContent {
  my $id = shift;
  my $result = OldPermanentAnchorsGetPageContent($id);
  if (not $result and $PermanentAnchors{$id}) {
    $result = OldPermanentAnchorsGetPageContent($PermanentAnchors{$id});
    $result =~ s/^(.*\n)*.*\[::$id\]// or return '';
    $result =~ s/(\n=|\n----|\[::$FreeLinkPattern\])(.*\n)*.*$//o;
  }
  return $result;
}

=head2 User Interface Changes

Some user interface changes are required as well.

=over

=item *

Allow the page index to list permanent anchors or not by setting
C<@IndexOptions>.

=cut

push(@IndexOptions, ['permanentanchors', T('Include permanent anchors'),
		     1, sub { keys %PermanentAnchors }]);

=item *

Make sure that you can view old revisions of pages that have a
permanent anchor of the same name. This requires link munging for all
browse links from C<GetHistoryLine>.

=back

=cut

*OldPermanentAnchorsGetHistoryLine = *GetHistoryLine;
*GetHistoryLine = *NewPermanentAnchorsGetHistoryLine;

sub NewPermanentAnchorsGetHistoryLine {
  my $id = shift;
  my $html = OldPermanentAnchorsGetHistoryLine($id, @_);
  if ($PermanentAnchors{$id}) {
    my $encoded_id = UrlEncode($id);
    # link to the current revision; ignore dependence on $UsePathInfo
    $html =~ s!$ScriptName[/?]$encoded_id!$ScriptName?action=browse;anchor=0;id=$encoded_id!;
    # link to old revisions
    $html =~ s!action=browse;id=$encoded_id!action=browse;anchor=0;id=$encoded_id!g;
  }
  return $html;
}
# Copyright (C) 2004  Brock Wilcox <awwaiid@thelackthereof.org>
# Copyright (C) 2006, 2007  Alex Schroeder <alex@gnu.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('questionasker.pl', 'QuestionAsker Extension', undef, '2.3.4-18-g66972c4');

use vars qw(@QuestionaskerQuestions
	    $QuestionaskerRememberAnswer
	    $QuestionaskerSecretKey
	    $QuestionaskerRequiredList
	    %QuestionaskerProtectedForms);

# A list of arrays. The first element in each array is a string, the
# question to be asked. The second element is a subroutine which is
# passed the answer as the first argument.
@QuestionaskerQuestions =
  (['What is the first letter of this question?' => sub { shift =~ /^\s*W\s*$/i }],
   ['How many letters are in the word "four"?' => sub { shift =~ /^\s*(4|four)\s*$/i }],
   ['Tell me any number between 1 and 10' => sub { shift =~ /^\s*([1-9]|10|one|two|three|four|five|six|seven|eight|nine|ten)\s*$/ }],
   ["How many lives does a cat have?" => sub { shift =~ /^\s*(7|seven|9|nine)\s*$/i }],
   ["What is 2 + 4?" => sub { shift =~ /^\s*(6|six)\s*$/i }],
  );

# The page name for exceptions, if defined. Every page linked to via
# WikiWord or [[free link]] is considered to be a page which needs
# questions asked. All other pages do not require questions asked. If
# not set, then all pages need questions asked.
$QuestionaskerRequiredList = '';

# If a user answers a question correctly, remember this in the cookie
# and don't ask any further questions. The name of the parameter in
# the cookie can be changed should a spam bot target this module
# specifically. Changing the secret key will force all users to answer
# another question.
$QuestionaskerRememberAnswer = 1;
$QuestionaskerSecretKey = 'question';

# Forms using one of the following classes are protected.
%QuestionaskerProtectedForms = ('comment' => 1,
				'edit upload' => 1,
				'edit text' => 1,);

push(@MyInitVariables, \&QuestionaskerInit);

sub QuestionaskerInit {
  $QuestionaskerRequiredList = FreeToNormal($QuestionaskerRequiredList);
  $AdminPages{$QuestionaskerRequiredList} = 1;
  $CookieParameters{$QuestionaskerSecretKey} = '';
  $InvisibleCookieParameters{$QuestionaskerSecretKey} = 1;
}

*OldQuestionaskerDoPost = *DoPost;
*DoPost = *NewQuestionaskerDoPost;

sub NewQuestionaskerDoPost {
  my(@params) = @_;
  my $id = FreeToNormal(GetParam('title', undef));
  my $preview = GetParam('Preview', undef); # case matters!
  my $question_num = GetParam('question_num', undef);
  my $answer = GetParam('answer', undef);
  unless (UserIsEditor()
	  or $QuestionaskerRememberAnswer && GetParam($QuestionaskerSecretKey, 0)
	  or $preview
	  or $QuestionaskerQuestions[$question_num][1]($answer)
	  or QuestionaskerException($id)) {
    print GetHeader('', T('Edit Denied'), undef, undef, '403 FORBIDDEN');
    print $q->p(T('You did not answer correctly.'));
    print GetFormStart(), QuestionaskerGetQuestion(1),
      (map { $q->input({-type=>'hidden', -name=>$_,
			-value=>UnquoteHtml(GetParam($_))}) }
       qw(title text oldtime summary recent_edit aftertext)), $q->end_form;
    PrintFooter();
    # logging to the error log file of the server
    # warn "Q: '$QuestionaskerQuestions[$question_num][0]', A: '$answer'\n";
    return;
  }
  # Set the secret key only if a question has in fact been answered
  if (not GetParam($QuestionaskerSecretKey, 0)
      and $QuestionaskerQuestions[$question_num][1]($answer)) {
    SetParam($QuestionaskerSecretKey, 1)
  }
  return (OldQuestionaskerDoPost(@params));
}

*OldQuestionaskerGetEditForm = *GetEditForm;
*GetEditForm = *NewQuestionaskerGetEditForm;

sub NewQuestionaskerGetEditForm {
  return QuestionAddTo(OldQuestionaskerGetEditForm(@_));
}

*OldQuestionaskerGetCommentForm = *GetCommentForm;
*GetCommentForm = *NewQuestionaskerGetCommentForm;

sub NewQuestionaskerGetCommentForm {
  return QuestionAddTo(OldQuestionaskerGetCommentForm(@_));
}

sub QuestionAddTo {
  my $form = shift;
  if (not $upload
      and not QuestionaskerException(GetId())
      and not $QuestionaskerRememberAnswer && GetParam($QuestionaskerSecretKey, 0)
      and not UserIsEditor()) {
    my $question = QuestionaskerGetQuestion();
    $form =~ s/(.*)<p>(.*?)<label for="username">/$1$question<p>$2<label for="username">/;
  }
  return $form;
}

sub QuestionaskerGetQuestion {
  my $need_button = shift;
  my $button = $need_button ? $q->submit(-value=>T('Go!')) : '';
  my $question_number = int(rand(scalar(@QuestionaskerQuestions)));
  return $q->div({-class=>'question'},
		 $q->p(T('To save this page you must answer this question:')),
		 $q->blockquote($q->p($QuestionaskerQuestions[$question_number][0]),
				$q->p($q->input({-type=>'text', -name=>'answer'}),
				      $q->input({-type=>'hidden', -name=>'question_num',
						 -value=>$question_number}),
				      $button)));
}

sub QuestionaskerException {
  my $id = shift;
  return 0 unless $QuestionaskerRequiredList and $id;
  my $data = GetPageContent($QuestionaskerRequiredList);
  if ($WikiLinks) {
    while ($data =~ /$LinkPattern/g) {
      return 0 if FreeToNormal($1) eq $id;
    }
  }
  if ($FreeLinks) {
    while ($data =~ /\[\[$FreeLinkPattern\]\]/g) {
      return 0 if FreeToNormal($1) eq $id;
    }
  }
  return 1;
}
# Copyright (C) 2006  Alex Schroeder <alex@emacswiki.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
#    Free Software Foundation, Inc.
#    59 Temple Place, Suite 330
#    Boston, MA 02111-1307 USA

AddModuleDescription('referrer-rss.pl', 'Comments on Automatic Link Back', undef, '2.3.4-18-g66972c4');

$Action{"refer-rss"} = \&DoRefererRss;

sub DoRefererRss {
  my $url = QuoteHtml($ScriptName);
  my $date = TimeToRFC822($LastUpdate);
  my $limit = GetParam("rsslimit", 15); # Only take the first 15 entries
  my $count = 0;
  print GetHttpHeader('application/xml');
  print qq{<?xml version="1.0" encoding="utf-8"?>};
  if ($RssStyleSheet =~ /\.(xslt?|xml)$/) {
    print qq{<?xml-stylesheet type="text/xml" href="$RssStyleSheet" ?>};
  } elsif ($RssStyleSheet) {
    print qq{<?xml-stylesheet type="text/css" href="$RssStyleSheet" ?>};
  }
  print qq{<rss version="2.0">
<channel>
<docs>http://blogs.law.harvard.edu/tech/rss</docs>
};
  print "<title>" . QuoteHtml($SiteName) . " " . T("Referrers") . "</title>\n";
  print "<link>$url?action=refer</link>\n";
  print "<description>" . QuoteHtml($SiteDescription) . "</description>\n";
  print "<pubDate>" . $date. "</pubDate>\n";
  print "<lastBuildDate>" . $date . "</lastBuildDate>\n";
  print "<generator>Oddmuse</generator>\n";
  if ($RssImageUrl) {
    print "<image>\n";
    print "<url>" . $RssImageUrl . "</url>\n";
    print "<title>" . QuoteHtml($SiteName) . "</title>\n";
    print "<link>" . $url . "</link>\n";
    print "</image>\n";
  }
  my %when = ();
  my %where = ();
  for my $id (AllPagesList()) {
    ReadReferers($id);
    # $Referers{url} = time for each $id
    foreach my $url (keys %Referers) {
      # $where{$url} = HomePage, AlexSchroeder, What_Is_A_Wiki
      push(@{$where{$url}}, $id);
      # $when{$url} = last time
      $when{$url} = $Referers{$url}
	if $when{$url} < $Referers{$url};
    }
  }
  foreach my $url (sort { $when{$b} <=> $when{$a} } keys %when) {
    print "\n<item>\n";
    print "<title>" . QuoteHtml($url) . "</title>\n";
    print "<link>" . QuoteHtml($url) . "</link>\n";
    print "<description>" . join(", ", map {
      QuoteHtml(GetPageLink($_));
    } @{$where{$url}}) . ", " . CalcDay($when{$url}) . " " . CalcTime($when{$url}) . "</description>\n";
    print "<pubDate>" . $date . "</pubDate>\n";
    print "</item>\n";
  }
  print "</channel>\n</rss>\n";
}
# Copyright (C) 2004, 2005, 2006, 2010  Alex Schroeder <alex@gnu.org>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('referrer-tracking.pl', 'Automatic Link Back', undef, '2.3.4-18-g66972c4');

use LWP::UserAgent;

push(@KnownLocks, "refer_*");

$Action{refer} = \&DoPrintAllReferers;

use vars qw($RefererDir $RefererTimeLimit $RefererLimit $RefererFilter
%Referers);

$RefererTimeLimit = 86400; # How long referrals shall be remembered in seconds
$RefererLimit	  = 15;	   # How many different referer shall be remembered
$RefererFilter    = 'ReferrerFilter'; # Name of the filter page
$RefererTitleLimit = 70;   # This is used to shorten long titles

push(@MyInitVariables, \&RefererInit);

sub RefererInit {
  $RefererFilter = FreeToNormal($RefererFilter); # spaces to underscores
  $AdminPages{$RefererFilter} = 1;
  $RefererDir  = "$DataDir/referer"; # Stores referer data
}

push(@MyAdminCode, \&RefererMenu);

sub RefererMenu {
  my ($id, $menuref, $restref) = @_;
  push(@$menuref, ScriptLink('action=refer', T('All Referrers'), 'refer'));
}

*RefererOldPrintFooter = *PrintFooter;
*PrintFooter = *RefererNewPrintFooter;

sub RefererNewPrintFooter {
  my ($id, $rev, $comment, @rest) = @_;
  if (not GetParam('embed', $EmbedWiki)) {
    my $referers = RefererTrack($id);
    print $referers if $referers;
  }
  RefererOldPrintFooter($id, $rev, $comment, @rest);
}

*RefererOldExpireKeepFiles = *ExpireKeepFiles;
*ExpireKeepFiles = *RefererNewExpireKeepFiles;

sub RefererNewExpireKeepFiles {
  RefererOldExpireKeepFiles(@_); # call with opened page
  ReadReferers($OpenPageName);   # clean up reading (expiring) and writing
  WriteReferers($OpenPageName);
}

*RefererOldDeletePage = *DeletePage;
*DeletePage = *RefererNewDeletePage;

sub RefererNewDeletePage {
  my $status = RefererOldDeletePage(@_);
  return $status if $status; # this would be the error message
  my $id = shift;
  my $fname = GetRefererFile($id);
  unlink($fname) if (-f $fname);
  return ''; # no error
}

## == Actual Code ==

sub GetRefererFile {
  my $id = shift;
  return "$RefererDir/$id.rf";
}

sub ReadReferers {
  my $file = GetRefererFile(shift);
  %Referers = ();
  if (-f $file) {
    my ($status, $data) = ReadFile($file);
    %Referers = split(/$FS/, $data, -1) if $status;
  }
  ExpireReferers();
}

sub ExpireReferers { # no need to save the pruned list if nothing else changes
  if ($RefererTimeLimit) {
    foreach (keys %Referers) {
      if ($Now - $Referers{$_} > $RefererTimeLimit) {
	delete $Referers{$_};
      }
    }
  }
  if ($RefererLimit) {
    my @list = sort {$Referers{$a} cmp $Referers{$b}} keys %Referers;
    @list = @list[$RefererLimit .. @list-1];
    foreach (@list) {
      delete $Referers{$_};
    }
  }
}

# maybe test for valid utf-8 later?

# http://www.w3.org/International/questions/qa-forms-utf-8

# $field =~
#   m/^(
#      [\x09\x0A\x0D\x20-\x7E]            # ASCII
#    | [\xC2-\xDF][\x80-\xBF]             # non-overlong 2-byte
#    |  \xE0[\xA0-\xBF][\x80-\xBF]        # excluding overlongs
#    | [\xE1-\xEC\xEE\xEF][\x80-\xBF]{2}  # straight 3-byte
#    |  \xED[\x80-\x9F][\x80-\xBF]        # excluding surrogates
#    |  \xF0[\x90-\xBF][\x80-\xBF]{2}     # planes 1-3
#    | [\xF1-\xF3][\x80-\xBF]{3}          # planes 4-15
#    |  \xF4[\x80-\x8F][\x80-\xBF]{2}     # plane 16
#   )*$/x;

sub UrlToTitle {
  my $title = QuoteHtml(shift);
  $title = $1 if $title =~ /$FullUrlPattern/; # extract valid URL
  $title =~ s/\%([0-9a-f][0-9a-f])/chr(hex($1))/egi; # decode if possible
  $title =~ s!^https?://!!;
  $title =~ s!\.html?$!!;
  $title =~ s!/$!!;
  # shorten it if necessary
  if (length($title) > $RefererTitleLimit) {
    $title = substr($title, 0, $RefererTitleLimit - 10)
      . "..." . substr($title, -7);
  }
  return $title;
}

sub GetReferers {
  my $result = join(' ', map {
    my ($ts, $title) = split(/ /, $Referers{$_}, 2);
    $title = UrlToTitle($_) unless $title;
    $q->a({-href=>$_}, $title);
  } keys %Referers);
  return $q->div({-class=>'refer'}, $q->p(T('Referrers') . ': ' . $result))
    if $result;
}

sub PageContentToTitle {
  my ($content) = @_;
  my $title = $1 if $content =~ m!<h1.*?>(.*?)</h1>!;
  $title = $1 if not $title and $content =~ m!<title>(.*?)</title>!;
  # get rid of extra tags
  $title =~ s!<.*?>!!g;
  # trimming
  $title =~ s!\s+! !g;
  $title =~ s!^ !!;
  $title =~ s! $!!;
  $title = substring($title, 0, $RefererTitleLimit) . "..."
    if length($title) > $RefererTitleLimit;
  return $title;
}

sub UpdateReferers {
  my $self = $ScriptName;
  my $referer = $q->referer();
  return  unless $referer and $referer !~ /$self/;
  foreach (split(/\n/,GetPageContent($RefererFilter))) {
    if (/^ ([^ ]+)[ \t]*$/) {  # only read lines with one word after one space
      my $regexp = $1;
      return  if $referer =~ /$regexp/i;
    }
  }
  my $ua = LWP::UserAgent->new;
  my $response = $ua->get($referer);
  return unless $response->is_success and $response->decoded_content =~ /$self/;
  my $title = PageContentToTitle($response->decoded_content);
  # starting with a timestamp makes sure that numerical comparisons still work!
  $Referers{$referer} = "$Now $title";
  return 1;
}

sub WriteReferers {
  my $id = shift;
  return unless RequestLockDir('refer_' . $id); # not fatal
  my $data = join($FS, %Referers);
  my $file = GetRefererFile($id);
  if ($data) {
    CreateDir($RefererDir);
    WriteStringToFile($file, $data);
  } else {
    unlink $file; # just try it, doesn't matter if it fails
  }
  ReleaseLockDir('refer_' . $id);
}

sub RefererTrack {
  my $id = shift;
  return unless $id;
  ReadReferers($id);
  WriteReferers($id) if UpdateReferers($id);
  return GetReferers();
}

sub DoPrintAllReferers {
  print GetHeader('', T('All Referrers'), ''), $q->start_div({-class=>'content refer'});
  PrintAllReferers(AllPagesList());
  print $q->end_div();
  PrintFooter();
}

sub PrintAllReferers {
  for my $id (@_) {
    ReadReferers($id);
    print $q->div({-class=>'page'},
		  $q->p(GetPageLink($id)),
		  GetReferers()) if %Referers;
  }
}
# Copyright (C) 2014  Alex Schroeder <alex@gnu.org>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

AddModuleDescription('smiles.pl', 'Smilies', undef, '2.3.4-18-g66972c4');

# The smilies are from the Emacs 24 distribution. There, you'll find
# them in the etc/images/smilies/medium directory.

# Files: blink.xpm braindamaged.xpm cry.xpm dead.xpm evil.xpm forced.xpm
#        frown.xpm grin.xpm indifferent.xpm reverse-smile.xpm sad.xpm
#        smile.xpm wry.xpm
# Author: Adam Sjøgren
# Copyright (C) 2007-2013 Free Software Foundation, Inc.
# License: GNU General Public License version 3 or later (see COPYING)

%Smilies = (
	    # blink
	    quotemeta(';-)') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAACFQTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0Aj3sA////jG0orwAAAAF0Uk5TAEDm2GYAAAABYktHRApo0PRWAAAAcklEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeXl7eXibAwAQUKBcvVWAQLk5xKxcvN2QQKS8UBzIcGUTLwSCQQbQQRIsHMoiUiJeXF7o7MgiXTywvlwQqZgqvFBScDtQONrAcaCDcCoSlUGcAAESjIP6OTV1GAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwBw7jJgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMHZTW5oAAAAASUVORK5CYII',
	    # braindamaged
	    quotemeta('8-)') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAB5QTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90Aj3sA////2fI0PgAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAeElEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJDuKOIoUibAwBQu5iiSWKrAIFwukpTmWG7IIFIu5pSSWO7IIFruKFIoUh7IIFpYDgTigQwiJeLl5YXujkDFjeXlEkDFTOEVgoLtQO2M6SA1QAPhViAshToDABSyHdgjFa3AAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwBw7jJgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMHZTW5oAAAAASUVORK5CYII',
	    # cry
	    quotemeta(':\'(') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAB5QTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0A////M/SGKgAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAcklEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeXl7eXibAwBRe0V4hUarAIFwuKFHYWG7IIAKUKXQsd2QQBTJKxMoDwQwgCARKFQsKmgOlhMsb3UskgIqZQoDKHV0VgAa2l5dXAA2EW4GwFOoMAEHVILjOIAB7AAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwBw7jJgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMHZTW5oAAAAASUVORK5CYII',
	    # dead
	    quotemeta('X-(') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAB5QTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90Aj3sA////2fI0PgAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAdklEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJDh7hFS0ibAwBQiIuLo6KrAIFzuGF4qUm7IIFIiIhro6O7IIFoBUtMeyCBaWA4E4oFAKfHy8kKglHB5Y3m5BFAxU3iFoGB7qQIDYzpITZkAwgqEpVBnAABAVB4+nUMqPgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMAcO4yYAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTQtMDctMDNUMTQ6NDc6NDYrMDI6MDB2U1uaAAAAAElFTkSuQmC',
	    # evil
	    quotemeta('>-{') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAACFQTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0Aj3sA////jG0orwAAAAF0Uk5TAEDm2GYAAAABYktHRApo0PRWAAAAc0lEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBuUV7eXCbAwBTuWF4uUqrAIFxe2O4hXm7IIFJe2AJkODKIloNBIINoIYgWD2QQKREvLy90dwQqnlheLglUzBReKSg4HaidMR2kBmgg3AqEpVBnAAAxxSDj4Zf1RAAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMAcO4yYAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTQtMDctMDNUMTQ6NDc6NDYrMDI6MDB2U1uaAAAAAElFTkSuQmC',
	    # forced
	    quotemeta(':-]') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAB5QTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0A////M/SGKgAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAb0lEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeUV7eXibAwBReWF4uXqrAIFwOYpQbMohAGI4MouVgEMggWgyizQMZRArdy8tLxB0ZhItNysudzQ0ZmEIUBQWFXBWABoLUAA2EW4GwFOoMAENHIDaBTwKaAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwBw7jJgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMHZTW5oAAAAASUVORK5CYII',
	    # frown
	    quotemeta('>-(') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAB5QTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0A////M/SGKgAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAbklEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBuUV7eXCbAwBTuWF4uUqrAIFxe2O4hXm7IIFJe2AJkODKIloNBIIIhUl4sKGgOlBIub3QvkQAqZgoRKS93dFUAGtheXl4BNBBuBcJSqDMAINwgfeXok+IAAAAldEVYdGRhdGU6Y3JlYXRlADIwMTQtMDctMDNUMTQ6NDc6NDYrMDI6MDAHDuMmAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwdlNbmgAAAABJRU5ErkJggg=',
	    # grin
	    quotemeta(':-D') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAACFQTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0A////j3sAOzDvXwAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAcElEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeUV7eXibAwBReWF4uXqrAIFwOYpQbMohAGI4MouVgEMggWiiWbJYoHsggUiI5c+ZEd0eg4oUzZ0oBFTOFVwkKLgdqZ0wHqQUaCLcCYSnUGQBvkCGbMSPrngAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMAcO4yYAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTQtMDctMDNUMTQ6NDc6NDYrMDI6MDB2U1uaAAAAAElFTkSuQmC',
	    # indifferent
	    quotemeta(':-|') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAB5QTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0A////M/SGKgAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAb0lEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeUV7eXibAwBReWF4uXqrAIFwOYpQbMohAGI4MouVgEMggmg6i3QIZRISFy4vLxR0ZhAsdTQQNzQ0ZmILLBd0FgdoZ00HagAbCrUBYCnUGABvIH6pXpKoIAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwBw7jJgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMHZTW5oAAAAASUVORK5CYII',
	    # reverse-smile
	    quotemeta('(-:') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAACFQTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90Aj3sAWU0A////QtT48gAAAAF0Uk5TAEDm2GYAAAABYktHRApo0PRWAAAAbklEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeDgRlAgxM4RWCgu2lCgzC5Y3l5RLlhgwiJeLl5YXujgyihSA14oEMouVgEMggUg4UEi93BCoGMwyB2kEMoHbG9Mry8ulAA+FWICyFOgMAVzMhig+T55IAAAAldEVYdGRhdGU6Y3JlYXRlADIwMTQtMDctMDNUMTQ6NDc6NDYrMDI6MDAHDuMmAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwdlNbmgAAAABJRU5ErkJggg=',
	    # sad
	    quotemeta(':-(') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAB5QTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0A////M/SGKgAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAaUlEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeUV7eXibAwBReWF4uXqrAIFwOYpQbMohAGI4MouVgEIhgiJQXCwqaA6WEyxvdSySAiplCRMrLHV0VgAa2l5dXAA2EW4GwFOoMAGqJITUDfchKAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwBw7jJgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMHZTW5oAAAAASUVORK5CYII',
	    # smile
	    quotemeta(':-)') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAACFQTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0Aj3sA////jG0orwAAAAF0Uk5TAEDm2GYAAAABYktHRApo0PRWAAAAbUlEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeUV7eXibAwBReWF4uXqrAIFwOYpQbMohAGI4MouVgEMggWgiixQMZRErEy8sL3R2BiieWl0sCFTOFVwoKTgdqZ0wHqQEaCLcCYSnUGQB7ciGbohFtcwAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMAcO4yYAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTQtMDctMDNUMTQ6NDc6NDYrMDI6MDB2U1uaAAAAAElFTkSuQmC',
	    # wry
	    quotemeta(':-/') => 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAABGdBTUEAALGPC/xhBQAAAAFzUkdCAK7OHOkAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAB5QTFRFAAAAAAAAHRkAiHUA07YA+tgAZFYA/90AWU0A////M/SGKgAAAAF0Uk5TAEDm2GYAAAABYktHRAnx2aXsAAAAZklEQVQI12NgYGAUFBRgAAJGZdcQIxBLLLy8vDQRKJBeUV7eXibAwBReWF4uXqrAIFwOYpQbMohAGI4MouVgEAhhlAEZIiCGGVBKuNxRUFAEqJgpPBkoBNTOmJ4OUiSAsAJhKdQZAIcwIb3IdkxFAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDE0LTA3LTAzVDE0OjQ3OjQ2KzAyOjAwBw7jJgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAxNC0wNy0wM1QxNDo0Nzo0NiswMjowMHZTW5oAAAAASUVORK5CYII',
           );
# Copyright (C) 2004, 2006  Alex Schroeder <alex@emacswiki.org>
#               2004  Sebastian Blatt <sblatt@havens.de>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
#    Free Software Foundation, Inc.
#    59 Temple Place, Suite 330
#    Boston, MA 02111-1307 USA

# Limits the number of parallel Oddmuse instances to
# $InstanceThrottleLimit by keeping track of the process ids in
# $InstanceThrottleDir

AddModuleDescription('throttle.pl', 'Limit Number Of Instances Running', undef, '2.3.4-18-g66972c4');

use File::Glob ':glob';
use vars qw($InstanceThrottleDir $InstanceThrottleLimit);

$InstanceThrottleDir = $DataDir."/pids"; # directory for pid files
$InstanceThrottleLimit = 2; # maximum number of parallel processes

*OldDoSurgeProtection = *DoSurgeProtection;
*DoSurgeProtection = *NewDoSurgeProtection;

*OldDoBrowseRequest = *DoBrowseRequest;
*DoBrowseRequest = *NewDoBrowseRequest;

sub NewDoSurgeProtection {
  DoInstanceThrottle();
  CreatePidFile();
  OldDoSurgeProtection();
}

sub NewDoBrowseRequest {
  OldDoBrowseRequest();
  RemovePidFile();
}

# limit the script to a maximum of $InstanceThrottleLimit instances
sub DoInstanceThrottle {
  my @pids = bsd_glob($InstanceThrottleDir."/*");
  # Go over all pids: validate each pid by sending signal 0, unlink
  # pidfile if pid does not exist and return 0. Count the number of
  # zeros (= removed files = zombies) with grep.
  my $zombies = grep /^0$/,
    (map {/(\d+)$/ and kill 0,$1 or unlink and 0} @pids);
  if (scalar(@pids)-$zombies >= $InstanceThrottleLimit) {
    ReportError(Ts('Too many instances.  Only %s allowed.',
		   $InstanceThrottleLimit),
                '503 Service Unavailable',
	       undef,
	       $q->p(T('Please try again later. Perhaps somebody is running maintenance or doing a long search. Unfortunately the site has limited resources, and so we must ask you for a bit of patience.')));
  }
}

sub CreatePidFile {
  CreateDir($InstanceThrottleDir);
  my $data = $q->request_method . ' ' . $q->url(-path_info=>1) . "\n";
  foreach my $param ($q->param) {
    next if $param eq 'pwd';
    $data .= "Param " . $param . "=" . $q->param($param) . "\n";
  }
  WriteStringToFile("$InstanceThrottleDir/$$", $data);
}

sub RemovePidFile {
  my $file = "$InstanceThrottleDir/$$";
  # not fatal
  unlink $file;
}
# Copyright (C) 2013  Alex Schroeder <alex@gnu.org>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

package OddMuse;

AddModuleDescription('toc-js.pl', 'Javascript Table of Contents Extension', undef, '2.3.4-18-g66972c4');

use vars qw($TocOutlineLibrary);

$TocOutlineLibrary = 'http://h5o.googlecode.com/files/outliner.0.5.0.62.js';

# Add the dojo script to edit pages.
push (@MyInitVariables, \&TocScript);

sub TocScript {
  # cookie is not initialized yet so we cannot use GetParam
  # Cross browser compatibility: http://www.tek-tips.com/faqs.cfm?fid=4862
  # HTML5 Outlines: http://blog.tremily.us/posts/HTML5_outlines/
  # Required library: http://code.google.com/p/h5o/
  if (GetParam('action', 'browse') eq 'browse') {
    $HtmlHeaders .= qq{
<script type="text/javascript" src="$TocOutlineLibrary"></script>
<script type="text/javascript">

  function addOnloadEvent(fnc) {
    if ( typeof window.addEventListener != "undefined" )
      window.addEventListener( "load", fnc, false );
    else if ( typeof window.attachEvent != "undefined" ) {
      window.attachEvent( "onload", fnc );
    }
    else {
      if ( window.onload != null ) {
	var oldOnload = window.onload;
	window.onload = function ( e ) {
	  oldOnload( e );
	  window[fnc]();
	};
      }
      else
	window.onload = fnc;
    }
  }

  // https://stackoverflow.com/questions/280634/endswith-in-javascript
  if (typeof String.prototype.endsWith !== 'function') {
    String.prototype.endsWith = function(suffix) {
      return this.indexOf(suffix, this.length - suffix.length) !== -1;
    };
  }

  var initToc=function() {

    var outline = HTML5Outline(document.body);
    if (outline.sections.length == 1) {
      outline.sections = outline.sections[0].sections;
    }

    if (outline.sections.length > 1
	|| outline.sections.length == 1
           && outline.sections[0].sections.length > 0) {

      var toc = document.getElementById('toc');

      if (!toc) {
	var divs = document.getElementsByTagName('div');
	for (var i = 0; i < divs.length; i++) {
	  if (divs[i].getAttribute('class') == 'toc') {
	    toc = divs[i];
	    break;
	  }
	}
      }

      if (!toc) {
	var h2 = document.getElementsByTagName('h2')[0];
	if (h2) {
	  toc = document.createElement('div');
	  toc.setAttribute('class', 'toc');
	  h2.parentNode.insertBefore(toc, h2);
	}
      }

      if (toc) {
        var html = outline.asHTML(true);
        toc.innerHTML = html;

	items = toc.getElementsByTagName('a');
	for (var i = 0; i < items.length; i++) {
	  while (items[i].textContent.endsWith('✎')) {
            var text = items[i].childNodes[0].nodeValue;
	    items[i].childNodes[0].nodeValue = text.substring(0, text.length - 1);
	  }
	}
      }
    }
  }

  addOnloadEvent(initToc);
  </script>
};
  }
}
# Copyright (C) 2004  Alex Schroeder <alex@emacswiki.org>
#               2004  Tilmann Holst <spam@tholst.de>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
#    Free Software Foundation, Inc.
#    59 Temple Place, Suite 330
#    Boston, MA 02111-1307 USA

AddModuleDescription('translations.pl', 'Translation Extension', undef, '2.3.4-18-g66972c4');

push(@MyRules, \&TranslationRule);

sub TranslationRule {
  if (m/\G(\&lt;translation +\[\[$FreeLinkPattern\]\] +(\d+)\&gt;[ \t]*)/gc) {
    Dirty($1);
    print GetTranslationLink($2, $3);
    return '';
  }
  return;
}

sub GetCurrentPageRevision {
  my $id   = shift;
  my %page = ParseData(ReadFileOrDie(GetPageFile($id)));
  return $page{revision};
}

sub GetTranslationLink {
  my ($id, $revision) = @_;
  my $result = "";
  my $currentRevision;
  $id =~ s/^\s+//;		# Trim extra spaces
  $id =~ s/\s+$//;
  $id     = FreeToNormal($id);
  $result = Ts('This page is a translation of %s. ', GetPageOrEditLink( $id, '', 0, 1));
  $currentRevision = GetCurrentPageRevision($id);

  if ($currentRevision == $revision) {
    $result .= T("The translation is up to date.");
  } elsif ( $currentRevision > $revision ) {
    $result .= T("The translation is outdated.") . ' '
      . ScriptLink("action=browse&diff=1&id=$id&revision=$currentRevision&diffrevision=$revision",
		   T("(diff)"));
  } else {
    $result .= T("The page does not exist.");
  }
  return $result;
}
#!/usr/bin/env perl
# ====================[ usemod.pl                          ]====================
AddModuleDescription('usemod.pl', 'Usemod Markup Extension', undef, '2.3.4-18-g66972c4');

use vars qw($RFCPattern $ISBNPattern @HtmlTags $HtmlTags $HtmlLinks $RawHtml
      $UseModSpaceRequired $UseModMarkupInTitles);

push(@MyRules, \&UsemodRule);
# The ---- rule conflicts with the --- rule in markup.pl and portrait-support.pl
# The == heading rule conflicts with the same rule in portrait-support.pl
# The : indentation rule conflicts with a similar rule in portrait-support.pl
$RuleOrder{\&UsemodRule} = 100;

$RFCPattern  = 'RFC\\s?(\\d+)';
$ISBNPattern = 'ISBN:?([0-9- xX]{10,14})';
$HtmlLinks   = 0;   # 1 = <a href="foo">desc</a> is a link
$RawHtml     = 0;   # 1 = allow <HTML> environment for raw HTML inclusion
@HtmlTags    = ();  # List of HTML tags.  If not set, determined by $HtmlTags
$HtmlTags    = 0;   # 1 = allow some 'unsafe' HTML tags
$UseModSpaceRequired = 1;  # 1 = require space after * # : ; for lists.
$UseModMarkupInTitles = 0; # 1 = may use links and other markup in ==titles==
$UseModExtraSpaceRequired = 0; # 1 = require space before : in definition lists

# do this later so that the user can customize some vars
push(@MyInitVariables, \&UsemodInit);

sub UsemodInit {
  if (not @HtmlTags) {   # do not override settings in the config file
    if ($HtmlTags) {    # allow many tags
      @HtmlTags = qw(b i u font big small sub sup h1 h2 h3 h4 h5 h6 cite code
         em s strike strong tt var div center blockquote ol ul dl
         table caption br p hr li dt dd tr td th);
    } else {      # only allow a very small subset
      @HtmlTags = qw(b i u em strong tt);
    }
  }
}

my $UsemodHtmlRegExp;
my $rowcount;

sub UsemodRule {
  $UsemodHtmlRegExp = join('|',(@HtmlTags)) unless $UsemodHtmlRegExp;
  # <pre> for monospaced, preformatted and escaped
  if ($bol && m/\G&lt;pre&gt;\n?(.*?\n)&lt;\/pre&gt;[ \t]*\n?/cgs) {
    return CloseHtmlEnvironments() . $q->pre({-class=>'real'}, $1) . AddHtmlEnvironment('p');
  }
  # <code> for monospaced and escaped
  elsif (m/\G\&lt;code\&gt;(.*?)\&lt;\/code\&gt;/cgis) { return $q->code($1); }
  # <nowiki> for escaped
  elsif (m/\G\&lt;nowiki\&gt;(.*?)\&lt;\/nowiki\&gt;/cgis) { return $1; }
  # whitespace for monospaced, preformatted and escaped, all clean
  # note that ([ \t]+(.+\n)*.*) seems to crash very long blocks (2000 lines and more)
  elsif ($bol && m/\G(\s*\n)*([ \t]+.+)\n?/gc) {
    my $str = $2;
    while (m/\G([ \t]+.*)\n?/gc) {
      $str .= "\n" . $1;
    }
    return OpenHtmlEnvironment('pre',1) . $str; # always level 1
  }
  # unumbered lists using *
  elsif ($bol && m/\G(\s*\n)*(\*+)[ \t]{$UseModSpaceRequired,}/cog
	 or InElement('li') && m/\G(\s*\n)+(\*+)[ \t]{$UseModSpaceRequired,}/cog) {
    return CloseHtmlEnvironmentUntil('li') . OpenHtmlEnvironment('ul',length($2))
      . AddHtmlEnvironment('li');
  }
  # numbered lists using #
  elsif ($bol && m/\G(\s*\n)*(\#+)[ \t]{$UseModSpaceRequired,}/cog
	 or InElement('li') && m/\G(\s*\n)+(\#+)[ \t]{$UseModSpaceRequired,}/cog) {
    return CloseHtmlEnvironmentUntil('li') . OpenHtmlEnvironment('ol',length($2))
      . AddHtmlEnvironment('li');
  }
  # indented text using : (use blockquote instead?)
  elsif ($bol && m/\G(\s*\n)*(\:+)[ \t]{$UseModSpaceRequired,}/cog
	 or InElement('dd') && m/\G(\s*\n)+(\:+)[ \t]{$UseModSpaceRequired,}/cog) {
    return CloseHtmlEnvironmentUntil('dd') . OpenHtmlEnvironment('dl',length($2), 'quote')
      . $q->dt() . AddHtmlEnvironment('dd');
  }
  # definition lists using ;
  elsif (($bol and m/\G(\s*\n)*(\;+)[ \t]{$UseModSpaceRequired,}(?=.*[ \t]{$UseModExtraSpaceRequired,}\:)/cog) or
         (InElement('dd') and m/\G(\s*\n)+(\;+)[ \t]{$UseModSpaceRequired,}(?=.*[ \t]{$UseModExtraSpaceRequired,}\:)/cog)) {
    return CloseHtmlEnvironmentUntil('dd')
      .OpenHtmlEnvironment('dl', length($2))
      .AddHtmlEnvironment('dt');  # `:' needs special treatment, later
  }
  elsif (InElement('dt') and m/\G(?<=[ \t]){$UseModExtraSpaceRequired,}:[ \t]*/cg) {
    return CloseHtmlEnvironmentUntil('dt')
      .CloseHtmlEnvironment()
      .AddHtmlEnvironment('dd');
  }
  # headings using = (with lookahead)
  elsif ($bol && $UseModMarkupInTitles
	 && m/\G(\s*\n)*(\=+)[ \t]*(?=[^=\n]+=)/cg) {
    my $depth = length($2);
    $depth = 6 if $depth > 6;
    $depth = 2 if $depth < 2;
    my $html = CloseHtmlEnvironments() . ($PortraitSupportColorDiv ? '</div>' : '')
      . AddHtmlEnvironment('h' . $depth);
    $PortraitSupportColorDiv = 0; # after the HTML has been determined.
    $PortraitSupportColor = 0;
    return $html;
  } elsif ($UseModMarkupInTitles
	   && (InElement('h1') || InElement('h2') || InElement('h3')
	       || InElement('h4') || InElement('h5') || InElement('h6'))
	   && m/\G[ \t]*=+\n?/cg) {
    return CloseHtmlEnvironments() . AddHtmlEnvironment('p');
  } elsif ($bol && !$UseModMarkupInTitles
	   && m/\G(\s*\n)*(\=+)[ \t]*(.+?)[ \t]*(=+)[ \t]*\n?/cg) {
    my $html = CloseHtmlEnvironments() . ($PortraitSupportColorDiv ? '</div>' : '')
      . WikiHeading($2, $3) . AddHtmlEnvironment('p');
    $PortraitSupportColorDiv = 0; # after the HTML has been determined.
    $PortraitSupportColor = 0;
    return $html;
  }
  # horizontal lines using ----
  elsif ($bol && m/\G(\s*\n)*----+[ \t]*\n?/cg) {
    my $html = CloseHtmlEnvironments() . ($PortraitSupportColorDiv ? '</div>' : '')
      . $q->hr() . AddHtmlEnvironment('p');
    $PortraitSupportColorDiv = 0;
    $PortraitSupportColor = 0;
    return $html;
  }
  # tables using || -- the first row of a table
  elsif ($bol && m/\G(\s*\n)*((\|\|)+)([ \t])*(?=.*\|\|[ \t]*(\n|$))/cg) {
    $rowcount = 1;
    return OpenHtmlEnvironment('table',1,'user')
      . AddHtmlEnvironment('tr', 'class="odd first"')
      . AddHtmlEnvironment('td', UsemodTableAttributes(length($2)/2, $4));
  }
  # tables using || -- end of the row and beginning of the next row
  elsif (InElement('td') && m/\G[ \t]*((\|\|)+)[ \t]*\n((\|\|)+)([ \t]*)/cg) {
    my $attr = UsemodTableAttributes(length($3)/2, $5);
    my $type = ++$rowcount % 2 ? 'odd' : 'even';
    $attr = " " . $attr if $attr;
    return qq{</td></tr><tr class="$type"><td$attr>};
  }
  # tables using || -- an ordinary table cell
  elsif (InElement('td') && m/\G[ \t]*((\|\|)+)([ \t]*)(?!(\n|$))/cg) {
    my $attr = UsemodTableAttributes(length($1)/2, $3);
    $attr = " " . $attr if $attr;
    return "</td><td$attr>";
  }
  # tables using || -- since "next row" was taken care of above, this must be the last row
  elsif (InElement('td') && m/\G[ \t]*((\|\|)+)[ \t]*/cg) {
    return CloseHtmlEnvironments() . AddHtmlEnvironment('p');
  }
  # RFC
  elsif (m/\G$RFCPattern/cog) { return &RFC($1); }
  # ISBN -- dirty because the URL translations will change
  elsif (m/\G($ISBNPattern)/cog) { Dirty($1); print ISBN($2); return ''; }
  # traditional wiki syntax closure for bold italic'''''
  elsif (InElement('strong') and InElement('em') and m/\G'''''/cg) { # close both
    return CloseHtmlEnvironment('strong').CloseHtmlEnvironment('em');
  }
  # traditional wiki syntax for '''bold'''
  elsif (m/\G'''/cg) { return AddOrCloseHtmlEnvironment('strong'); }
  # traditional wiki syntax for ''italic''
  elsif (m/\G''/cg ) { return AddOrCloseHtmlEnvironment('em'); }
  # <html> for raw html
  elsif ($RawHtml && m/\G\&lt;html\&gt;(.*?)\&lt;\/html\&gt;/cgis) {
    return UnquoteHtml($1);
  }
  # miscellaneous html tags
  elsif (m/\G\&lt;($UsemodHtmlRegExp)(\s+[^<>]*?)?\&gt;/cogi) { 
    return AddHtmlEnvironment($1, $2); }
  elsif (m/\G\&lt;\/($UsemodHtmlRegExp)\&gt;/cogi) {
    return CloseHtmlEnvironment($1); }
  elsif (m/\G\&lt;($UsemodHtmlRegExp) *\/\&gt;/cogi) {
    return "<$1 />"; }
  # <a ...>text</a> for html links
  elsif ($HtmlLinks && m/\G\&lt;a(\s[^<>]+?)\&gt;(.*?)\&lt;\/a\&gt;/cgi) {
    return "<a$1>$2</a>";
  }
  return;
}

sub UsemodTableAttributes {
  my ($span, $left, $right) = @_;
  my $attr = '';
  $attr = "colspan=\"$span\"" if ($span != 1);
  m/\G(?=.*?([ \t]*)\|\|)/;
  $right = $1;
  $attr .= ' ' if ($attr and ($left or $right));
  if ($left and $right) { $attr .= 'align="center"' }
  elsif ($left ) { $attr .= 'align="right"' }
  elsif ($right) { $attr .= 'align="left"' }
  return $attr;
}

sub WikiHeading {
  my ($depth, $text) = @_;
  $depth = length($depth);
  $depth = 6 if $depth > 6;
  $depth = 2 if $depth < 2;
  return "<h$depth>$text</h$depth>";
}

sub RFC {
  my $num = shift;
  return $q->a({-href=>"http://tools.ietf.org/html/rfc${num}"}, "RFC $num");
}

sub ISBN {
  my $rawnum = shift;
  my $num = $rawnum;
  my $rawprint = $rawnum;
  $rawprint =~ s/ +$//;
  $num =~ s/[- ]//g;
  my $len = length($num);
  return "ISBN $rawnum" unless $len == 10 or $len == 13 or $len = 14; # be prepared for 2007-01-01
  my $first  = $q->a({-href => Ts('http://search.barnesandnoble.com/booksearch/isbninquiry.asp?ISBN=%s', $num)},
      "ISBN " . $rawprint);
  my $second = $q->a({-href => Ts('http://www.amazon.com/exec/obidos/ISBN=%s', $num)},
      T('alternate'));
  my $third  = $q->a({-href => Ts('http://www.pricescan.com/books/BookDetail.asp?isbn=%s', $num)},
      T('search'));
  my $html = "$first ($second, $third)";
  $html .= ' '  if ($rawnum =~ / $/);  # Add space if old ISBN had space.
  return $html;
}

=head1 COPYRIGHT AND LICENSE

The information below applies to everything in this distribution,
except where noted.

Copyright 2008, 2009, 2010 by Alex Schroeder <alex@gnu.org>.
Copyleft  2008 by Brian Curry <http://raiazome.com>.
Copyright 2008 by Weakish Jiang <weakish@gmail.com>.
Copyright 2004, 2005, 2006, 2007 by Alex Schroeder <alex@gnu.org>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see L<http://www.gnu.org/licenses/>.

=cut
