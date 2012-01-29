mod_log_chat_mysql
============

Developed by Jérôme Sautret <jerome.sautret@process-one.net>, Adapted for DB Logging by Michael Weibel <michael.weibel@amiadogroup.com>.

Prerequisite
============
In order to compile Emysql, Erlang R13 or newer will need to be installed.
If you are using the 2.1.10 ejabberd installer, that comes with an old version of Erlang and you will not be able to compile Emysql.

Installation
============
  * Download Emysql: https://github.com/Eonblast/Emysql
  * cd Emysql && make
  * In another directory download ejabbed-modules from subversion: svn co https://svn.process-one.net/ejabberd-modules
  * Copy Emysql/ebin/* to your ejabberd-modules (ebin) folder: cp Emysql/ebin/* ejabberd-modules/src/ejabberd-modules/ejabberd-dev/trunk/ebin
  * Make sure that emysql.app is present. If not build the Emysql module again.
  * Download mod_log_chat_mysql5 : git clone https://github.com/candy-chat/mod_log_chat_mysql5.git
  * Move the mod_log_chat_mysql5 directory into the root of the ejabberd-modules folder
  * Navigate to the mod_log_chat_mysql5 directory in the ejabberd-modules folder and call ./build.sh
  * If successful the module has been compiled and output to ebin/mod_log_chat_mysql5.beam. Copy this file to your ejabberd system ebin folder folder (e.g. /usr/lib/ejabberd/ebin on Debian)
  * Copy all the Emysql files to your ejabberd system ebin folder as well. (Emysql/ebin/*)
  * Create required mysql table like this

```sql
    CREATE TABLE mod_log_chat_mysql5 (
       id INT AUTO_INCREMENT PRIMARY KEY,
       fromJid VARCHAR(255) NOT NULL, 
       toJid VARCHAR(255) NOT NULL, 
       sentDate TIMESTAMP NOT NULL, 
       body TEXT, 
       type VARCHAR(10)
    ) ENGINE=MyISAM CHARACTER SET utf8;
```
  * See conf/ejabberd.conf.sample for an example configuration
  * Once the ejabberd module is loaded and you have started ejabberd.  Look at the log files to see if the module has been correctly started (erlang.log) 
