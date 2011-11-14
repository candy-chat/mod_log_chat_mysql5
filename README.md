mod_log_chat_mysql
============

Developed by Jérôme Sautret <jerome.sautret@process-one.net>, Adapted for DB Logging by Michael Weibel <michael.weibel@amiadogroup.com>.

Installation
============
  * Download Emysql: https://github.com/Eonblast/Emysql
  * cd Emysql && make
  * Copy ebin/* to your ejabberd-modules (ebin) folder
  *
  * Copy the whole directory to your ejabberd-modules directory
  * call ./build.sh
  * copy ebin/mod_log_chat_mysql5.beam to your modules folder (e.g. /usr/lib/ejabberd/ebin on Debian)
  * create required mysql table like this

    CREATE TABLE mod_log_chat_mysql5 (
       id INT AUTO_INCREMENT PRIMARY KEY,
       fromJid VARCHAR(255) NOT NULL, 
       toJid VARCHAR(255) NOT NULL, 
       sentDate TIMESTAMP NOT NULL, 
       body TEXT, 
       type VARCHAR(10)
    ) ENGINE=MyISAM CHARACTER SET utf8;

  * See conf/ejabberd.conf.sample for an example configuration
