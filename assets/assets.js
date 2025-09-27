import user_image from './user-image.png';
import code_icon from './code-icon.png';
import code_icon_dark from './code-icon-dark.png';
import edu_icon from './edu-icon.png';
import edu_icon_dark from './edu-icon-dark.png';
import project_icon from './project-icon.png';
import project_icon_dark from './project-icon-dark.png';
import vscode from './vscode.png';
import firebase from './firebase.png';
import firebase_dark from './firebase_dark.png';
import figma from './figma.png';
import git from './git.png';
import right_arrow_white from './right-arrow-white.png';
import logo from './logo.png';
import logo_dark from './logo_dark.png';
import mail_icon from './mail_icon.png';
import mail_icon_dark from './mail_icon_dark.png';
import profile_img from './profile-img.png';
import download_icon from './download-icon.png';
import hand_icon from './hand-icon.png';
import header_bg_color from './header-bg-color.png';
import moon_icon from './moon_icon.png';
import sun_icon from './sun_icon.png';
import arrow_icon from './arrow-icon.png';
import arrow_icon_dark from './arrow-icon-dark.png';
import menu_black from './menu-black.png';
import menu_white from './menu-white.png';
import close_black from './close-black.png';
import close_white from './close-white.png';
import web_icon from './web-icon.png';
import mobile_icon from './mobile-icon.png';
import ui_icon from './ui-icon.png';
import graphics_icon from './graphics-icon.png';
import right_arrow from './right-arrow.png';
import send_icon from './send-icon.png';
import right_arrow_bold from './right-arrow-bold.png';
import right_arrow_bold_dark from './right-arrow-bold-dark.png';
import image_linkedin from './image_linkedin.png';
import creative_vision from './creative-vision.png';
import innovation from './innovation.jpg';
import technical from './technical.jpg';
import etoile from './etoile.jpg';
import linux from './linux.jpg';

export const assets = {
    user_image,
    code_icon,
    code_icon_dark,
    edu_icon,
    edu_icon_dark,
    project_icon,
    project_icon_dark,
    vscode,
    firebase,
    firebase_dark,
    figma,
    git,
    right_arrow_white,
    logo,
    logo_dark,
    mail_icon,
    mail_icon_dark,
    profile_img,
    download_icon,
    hand_icon,
    header_bg_color,
    moon_icon,
    sun_icon,
    arrow_icon,
    arrow_icon_dark,
    menu_black,
    menu_white,
    close_black,
    close_white,
    web_icon,
    mobile_icon,
    ui_icon,
    graphics_icon,
    right_arrow,
    send_icon,
    right_arrow_bold,
    right_arrow_bold_dark,
    image_linkedin,
    creative_vision,
    innovation,
    technical,
    etoile,
    linux
};

export const workData = [
    {
        title: 'Frontend project',
        description: 'Web Design',
        bgImage: '/work-1.png',
    },
    {
        title: 'Geo based app',
        description: 'Mobile App',
        bgImage: '/work-2.png',
    },
    {
        title: 'Photography site',
        description: 'Web Design',
        bgImage: '/work-3.png',
    },
    {
        title: 'UI/UX designing',
        description: 'UI/UX Design',
        bgImage: '/work-4.png',
    },
]

export const serviceData = [
    { 
        icon: assets.code_icon, 
        title: '13 Matches - Java Game', 
        description: 'Strategic game developed in Java where two players compete by removing matches. Implementation of game algorithms and graphical interface.', 
        link: 'https://github.com/AstA6XD9/my-projects/tree/main/projet-court' 
    },
    { 
        icon: assets.mobile_icon, 
        title: 'Audio Application - Sound Game', 
        description: 'Interactive application developed in Java for creating and manipulating sounds. Intuitive user interface for audio editing and music creation.', 
        link: 'https://github.com/AstA6XD9/my-projects/tree/main/src' 
    },
    { 
        icon: assets.code_icon, 
        title: 'Data Compression - Ada', 
        description: 'Data compression algorithm implemented in Ada using Huffman coding method. Performance optimization and memory management for compressing large files.', 
        link: 'https://github.com/AstA6XD9/my-projects' 
    },
    { 
        icon: assets.web_icon, 
        title: 'Web Portfolio - Next.js', 
        description: 'Personal portfolio website developed with Next.js and Tailwind CSS. Responsive design with dark/light mode and smooth animations.', 
        link: 'https://github.com/AstA6XD9/portfolio' 
    },
]

export const infoList = [
    { icon: assets.code_icon, iconDark: assets.code_icon_dark, title: 'Languages', description: 'Python, Ada, Java, C,Ocaml' },
    { icon: assets.edu_icon, iconDark: assets.edu_icon_dark, title: 'Education', description: 'ENSEEIHT in Computer Science' },
    { icon: assets.project_icon, iconDark: assets.project_icon_dark, title: 'Projects', description: 'Built more than 3 projects' }
];

export const toolsData = [
    assets.vscode, assets.linux, assets.firebase, assets.figma, assets.git
];