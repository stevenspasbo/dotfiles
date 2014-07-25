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

desc "Installs bash files"
task :bashrc do

end

desc "Installs vim files"
task :vim do
  vimrc = "#{home_dir}/.vimrc"
  vim_dir = "#{home_dir}/.vim"
  if File.exists?(vimrc)
    puts "[ #{File.basename(vimrc)} found ]".colorize(:red)
    unless File.symlink?(vimrc)
      backup(vimrc)
      puts "\tMoved to $repoLocation/backup/ "
    else
      puts "\tFile is symlink, skipping backup "
    end
  else
    puts "[ No .vimrc file found ]"
    File.symlink("#{Dir.pwd}/vimrc", vimrc)
    puts "[ * ~/.vimrc symlink created ]"
  end
end

desc "Updates all vim plugins"
task :update_vim_plugins => :vim do

end

desc "Backs up dotfiles to $REPO/Backup"
task :backup do
  Dir.glob('*').each do |file|
    unless rakefile?(file) && markdown?(file) && file != "backups"
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
          begin
            t = Time.new
            date_str = "#{t.month}_#{t.day}_#{t.year}_-_#{t.hour}_#{t.min}_#{t.sec}"
            backupdir = BACKUP_DIR + "/" + date_str
            Dir.mkdir(backupdir) unless File.exists?(backupdir)
            puts "\t#{home_file} found. Backing up."
            File.rename(home_file, "#{File.expand_path(backupdir)}/#{base_dot_name}")
            File.symlink(File.expand_path(file), home_file)
          rescue SystemCallError => e
            puts e.message
          end
        end
      end
    end
  end
end

task :dir do
  puts File.dirname(__FILE__)
  puts __FILE__
end

#-------------------------------------------------------------
# Methods
#-------------------------------------------------------------

def backup(file)
  unless File.symlink?(file)
    puts blah
  end
end

def rakefile?(file)
  File.expand_path(file) == File.expand_path(__FILE__)
end

def markdown?(file)
  File.extname(file).downcase == ".md"
end
