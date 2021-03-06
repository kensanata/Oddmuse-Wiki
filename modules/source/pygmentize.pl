# Copyright (C) 2015  Alex-Daniel Jakimenko <alex.jakimenko@gmail.com>
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

use strict;
# use warnings;
use v5.10;
use utf8;

AddModuleDescription('pygmentize.pl', 'Pygmentize Extension', undef, '2.3.13-13-g8819183b');

our ($q, $bol, @KnownLocks, %RuleOrder, @MyRules, $TempDir, @MyInitVariables);

# You can push other stuff to that list.
# For example: push @PygmentizeArgs, qw(-F whitespace:spaces=true,tabs=true)
# If you want to change existing options then just reinitialize the list
our @PygmentizeArgs = qw(-O noclasses);

push(@MyInitVariables, sub {
  push(@KnownLocks, 'pygmentize');
     });

push(@MyRules, \&PygmentizeRule);
$RuleOrder{\&PygmentizeRule} = -60;

sub PygmentizeRule {
  if ($bol && m/\G\{\{\{(\w+)?[ \t]*\n(.*?)\n\}\}\}[ \t]*(\n|$)/cgs) {
    my $lexer = $1;
    my $contents = $2;
    return CloseHtmlEnvironments() . DoPygmentize($contents, $lexer) . AddHtmlEnvironment('p');
  }
  return;
}

sub DoPygmentize {
  my ($contents, $lexer) = @_;
  $lexer = "-l \Q$lexer\E" if $lexer; # should be already safe, but \Q \E just because I'm paranoid
  $lexer ||= '-g'; # -g for autodetect
  my $args = join ' ', map { quotemeta } @PygmentizeArgs;
  CreateDir($TempDir);
  $contents = UnquoteHtml($contents);

  RequestLockDir('pygmentize') or return '';
  WriteStringToFile("$TempDir/pygmentize", $contents);
  my $output = decode_utf8(`pygmentize $lexer -f html -O encoding=utf8 $args -- \Q$TempDir/pygmentize\E  2>&1`);
  ReleaseLockDir('pygmentize');

  if ($?) {
    $output = $q->p($q->strong($output)) # "sh: pygmentize: command not found"
        . $q->pre($contents);
  }
  return $output;
}
