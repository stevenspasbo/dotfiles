require 'colorize'

#-------------------------------------------------------------
# Tasks
#-------------------------------------------------------------
home_dir = ENV['HOME']

task :default do
  puts "Rakefile for my dotfiles. WIP. (default task)"
end

desc "Installs all dotfiles"
task :install do

end

desc "Installs bash files"
task :bashrc do
  puts "Installing bashrc"
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
  t = Time.new
  date_str = "#{t.month}_#{t.day}_#{t.year}_-_#{t.hour}_#{t.min}_#{t.sec}"
  begin
    backupdir = "#{File.expand_path(File.dirname(__FILE__)}/backups/#{date_str})"
    Dir.mkdir(backupdir) unless File.exists?(backupdir)
    Dir.glob('*').each do |file|
      unless rakefile?(file) && markdown?(file)
        home_file = "#{ENV['HOME']}/.#{file}"
        if File.exists?(home_file) && !File.symlink?(home_file)
          puts "#{home_file} found. Backing up."
          mv (File.expand_path(home_file), File.expand_path(backup_dir))
        end
      end
    end
  rescue SystemCallError => e
    puts e.message
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
