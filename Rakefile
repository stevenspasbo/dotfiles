#-------------------------------------------------------------
# Constants
#-------------------------------------------------------------
HOME = ENV['HOME']
REPO = File.expand_path(File.dirname(__FILE__))
BACKUP_DIR = REPO + "/backups"
TIME = Time.new
DATE_TIME_STRING = "#{TIME.month}_#{TIME.day}_#{TIME.year}_-_#{TIME.hour}_#{TIME.min}_#{TIME.sec}"
BACKUP_DIRECTORY = BACKUP_DIR + "/" + DATE_TIME_STRING

#-------------------------------------------------------------
# Methods
#-------------------------------------------------------------
def backup(file)
  new_file_name = "#{File.expand_path(BACKUP_DIRECTORY)}/#{File.basename(file)}"
  begin
    Dir.mkdir(BACKUP_DIRECTORY) unless File.exists?(BACKUP_DIRECTORY)
    unless File.exists?(new_file_name)
      File.rename(file, new_file_name)
    else
      File.rename(file, "#{new_file_name}#{rand(10000)}")
    end
  rescue SystemCallError => e
    puts e.message
  end
  puts "#{file} was moved to #{BACKUP_DIRECTORY}"
end

def rakefile?(file)
  File.expand_path(file) == File.expand_path(__FILE__)
end

def markdown?(file)
  File.extname(file).downcase == ".md"
end

def file_already_linked?(file)
  File.readlink(home_file) == File.expand_path(file)
end

#-------------------------------------------------------------
# Variables
#-------------------------------------------------------------
files = Dir.glob("*").each.reject do |file|
  file == "backups" ||
  markdown?(file) ||
  rakefile?(file)
end

#-------------------------------------------------------------
# Tasks
#-------------------------------------------------------------
task :default do
  puts "Rakefile for my dotfiles. WIP. (default task)"
end

desc "Updates all the vim plugins"
task :update_vim do
  `git submodule foreach git pull origin master`
end

desc "Backs up dotfiles to #{REPO}backup"
task :backup do
  files.each do |file|
    home_file = "#{HOME}/.#{file}"
    base_dot_name = File.basename(home_file)
    puts "Checking for #{base_dot_name}"
    if (File.exists?(home_file) && !File.symlink?(home_file))
      while (true)
        print "\t#{base_dot_name} exists. Backup and remove from home? (yes/no): "
        ans = $stdin.gets.chomp.downcase
        if (ans == "yes" || ans == "y")
          backup(home_file)
          next
        elsif (ans == "no" || ans == "n")
          files.delete(file)
          puts "\tMoving to next file"
          next
        end
      end
    end
    elsif (File.symlink?(file) && !file_already_linked(file))
      puts "#{base_dot_file} is a symlink to #{File.readlink(home_file)}"
      while (true)
        print "\tRemove symlink? File will not be deleted. (yes/no): "
        ans = $stdin.gets.chomp.downcase
        if (ans == "yes" || ans == "y")
          File.delete(home_file)
          next
        elsif (ans == "no" || ans == "n")
          files.delete(file)
          puts "\tMoving to next file"
          next
        end
    end
end

desc "Installs all dotfiles"
task :install => :backup do
  files.each do |file|
    home_file = "#{HOME}/.#{file}"
    base_dot_name = File.basename(home_file)
    puts "Creating symlink for #{base_dot_name} ..."
    if File.exists?(home_file)
      puts "\t#{base_dot_name} exists"
      if File.symlink?(home_file)
        if (file_already_linked?(home_file))
          puts "\tSymlink to file already exists. Moving on."
        end
      else
        File.symlink(File.expand_path(file), home_file)
      end
    end
  end
end


