#require 'CFPropertyList'

#-------------------------------------------------------------
# Constants
#-------------------------------------------------------------
HOME = ENV['HOME']
REPO = File.expand_path(File.dirname(__FILE__))
BACKUP_DIR = REPO + "/backups"
REPO_FONT_DIR = REPO + "/fonts"
USER_FONT_DIR = "#{ENV['HOME']}/Library/Fonts"
TIME = Time.new
DATE_TIME_STRING = "#{TIME.month}_#{TIME.day}_#{TIME.year}_-_#{TIME.hour}_#{TIME.min}_#{TIME.sec}"

#-------------------------------------------------------------
# Methods
#-------------------------------------------------------------
def backup(file)
  new_file_name = "#{File.expand_path(BACKUP_DIR)}/#{File.basename(file)}#{DATE_TIME_STRING}"
  begin
    Dir.mkdir(BACKUP_DIR) unless Dir.exists?(BACKUP_DIR)
    unless File.exists?(new_file_name)
      File.rename(file, new_file_name)
    else
      File.rename(file, "#{new_file_name}#{rand(10000)}")
    end
  rescue SystemCallError => e
    puts e.message
  end
  puts "\t#{file} was moved to #{new_file_name}"
end

#-------------------------------------------------------------
# Variables
#-------------------------------------------------------------
files = Dir.glob("dotfiles/*")

#-------------------------------------------------------------
# Tasks
#-------------------------------------------------------------
task :default do
  puts "Run 'rake -T' to see the list of available tasks"
end

desc "Updates all the vim plugins"
task :update_vim do
  sh "git submodule foreach git pull origin master"
end

desc "Backs up dotfiles to #{REPO}backup"
task :backup do
  files.each do |file|
    home_file = "#{HOME}/.#{File.basename(file)}"
    base_dot_name = File.basename(home_file)
    puts "Checking for #{base_dot_name}"
    if (File.exists?(home_file) && !File.symlink?(home_file))
      while (true)
        print "\t#{base_dot_name} exists. Backup and remove from home? (yes/no): "
        ans = $stdin.gets.chomp.downcase
        if (ans == "yes" || ans == "y")
          backup(home_file)
          break
        elsif (ans == "no" || ans == "n")
          files.delete(file)
          puts "\tMoving to next file"
          break
        end
      end
    elsif File.symlink?(home_file)
      puts "#{base_dot_name} is a symlink to #{File.readlink(home_file)}"
      while (true)
        print "\tRemove symlink? File will not be deleted. (yes/no): "
        ans = $stdin.gets.chomp.downcase
        if (ans == "yes" || ans == "y")
          File.delete(home_file)
          break
        elsif (ans == "no" || ans == "n")
          files.delete(file)
          puts "\tMoving to next file"
          break
        end
      end
    end
  end
end

desc "Installs all dotfiles"
task :install_dotfiles => :backup do
  files.each do |file|
    home_file = "#{HOME}/.#{File.basename(file)}"
    base_dot_name = File.basename(home_file)
    puts "Creating symlink for #{base_dot_name} ..."
    if File.exists?(home_file)
      puts "\t#{base_dot_name} exists"
      if File.symlink?(home_file)
        if (file_already_linked?(home_file))
          puts "\tSymlink to file already exists. Moving on."
        end
      end
    else
      File.symlink(File.expand_path(file), home_file)
    end
  end
end

desc "Installs RVM"
task :install_rvm do
  unless File.exists? "#{ENV['HOME']}/.rvm"
    print "Install RVM? (y/n) "
    ans = gets.downcase.chomp
    if (ans == "y" || ans == "yes")
      sh "curl -sSL https://get.rvm.io | bash"
    end
  else
    puts "RVM already installed"
  end
end

desc "Installs Homebrew"
task :install_homebrew do
  unless File.exists?("/usr/local/bin/brew")
    print "Install Homebrew? (y/n) "
    ans = gets.downcase.chomp
    if (ans == "y" || ans == "yes")
      sh 'ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"'
    end
  else
    puts "Homebrew already installed"
  end
end

desc "Installs everything"
task :install_all => [ :install_rvm, :install_homebrew, :install_dotfiles ] do
  puts "Installing everything..."
end

desc "Installs fonts"
task :install_fonts do
  if ((/darwin/ =~ RUBY_PLATFORM) != nil)
    Dir["#{REPO_FONT_DIR}/*"].each do |font|
      unless (File.exists? "#{USER_FONT_DIR}/#{File.basename(font)}")
        FileUtils.cp(font, USER_FONT_DIR)
      else
        puts "#{File.basename(font)} already exists in Users library"
      end
    end
    print "Fonts now installed." 
  end
end


