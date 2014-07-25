require 'colorize'

#-------------------------------------------------------------
# Variables
#-------------------------------------------------------------
HOME = ENV['HOME']
REPO = File.expand_path(File.dirname(__FILE__))
BACKUP_DIR = REPO + "/backups"

#-------------------------------------------------------------
# Tasks
#-------------------------------------------------------------
task :default do
  puts "Rakefile for my dotfiles. WIP. (default task)"
end

desc "Installs all dotfiles"
task :install do

end

desc "Updates all vim plugins"
task :update_vim_plugins => :vim do

end

desc "Backs up dotfiles to $REPO/Backup"
task :backup do
  Dir.glob('*').each do |file|
    unless rakefile?(file) && markdown?(file)
      next if file == "backups"
      home_file = "#{HOME}/.#{file}"
      base_dot_name = File.basename(home_file)
      puts "Checking for #{base_dot_name}"
      if File.exists?(home_file)
        puts "\t#{base_dot_name} exists"
        if File.symlink?(home_file)
          if (File.readlink(home_file) == File.expand_path(file))
            puts "\tSymlink to file already exists. Moving on."
          end
        else
          backup(home_file)
          File.symlink(File.expand_path(file), home_file)
        end
      end
    end
  end
end

#-------------------------------------------------------------
# Methods
#-------------------------------------------------------------

def backup(file)
  t = Time.new
  date_str = "#{t.month}_#{t.day}_#{t.year}_-_#{t.hour}_#{t.min}_#{t.sec}"
  backupdir = BACKUP_DIR + "/" + date_str
  new_file_name = "#{File.expand_path(backupdir)}/#{file}"
  begin
    Dir.mkdir(backupdir) unless File.exists?(backupdir)
    unless File.exists?(new_file_name)
      File.rename(file, new_file_name)
    else
      File.rename(file, "#{new_file_name}#{rand(10000)}")
    end
  rescue SystemCallError => e
    puts e.message
  end
end

def rakefile?(file)
  File.expand_path(file) == File.expand_path(__FILE__)
end

def markdown?(file)
  File.extname(file).downcase == ".md"
end
