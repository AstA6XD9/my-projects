'use client'
import Navbar from "./components/Navbar";
import  Header  from "./components/Header";
import About from "./components/About";
import Projects from "./components/Projects";
import Inspiration from "./components/Inspiration";
import Contact from "./components/Contact";
import Footer from "./components/Footer";
import { useState, useEffect } from "react";


export default function Home() {
  const [isDarkMode, setIsDarkMode] = useState(false);
  
  useEffect(() => {
    // Appliquer le style directement au body
    const body = document.body;
    if (isDarkMode) {
      body.style.backgroundColor = '#111827'; // gray-900
      body.style.color = 'white';
      document.documentElement.classList.add('dark');
      localStorage.setItem('theme', 'dark');
    } else {
      body.style.backgroundColor = '#f9fafb'; // gray-50 au lieu de blanc pur
      body.style.color = 'black';
      document.documentElement.classList.remove('dark');
      localStorage.setItem('theme', 'light');
    }
    
    // Forcer la couleur du texte et background sur tous les éléments
    const allElements = document.querySelectorAll('*');
    allElements.forEach(element => {
      if (isDarkMode) {
        element.style.color = 'white';
        // Forcer le background sur tous les éléments
        if (element.tagName === 'DIV' || element.tagName === 'SECTION' || element.tagName === 'MAIN') {
          element.style.backgroundColor = '#111827';
        }
        // Changer tous les blancs en gris
        if (element.style.backgroundColor === 'white' || element.style.backgroundColor === 'rgb(255, 255, 255)') {
          element.style.backgroundColor = '#374151'; // gray-700
        }
      } else {
        element.style.color = 'black';
        // Forcer le background sur tous les éléments
        if (element.tagName === 'DIV' || element.tagName === 'SECTION' || element.tagName === 'MAIN') {
          element.style.backgroundColor = '#f9fafb'; // gray-50 au lieu de blanc pur
        }
      }
    });
    
    // Forcer le style de la navbar
    const navbar = document.querySelector('nav');
    if (navbar) {
      if (isDarkMode) {
        navbar.style.backgroundColor = '#111827';
        navbar.style.color = 'white';
      } else {
        navbar.style.backgroundColor = 'white';
        navbar.style.color = 'black';
      }
    }
  }, [isDarkMode]);
  return (
    <>
      <div id="top" />
      <Navbar isDarkMode={isDarkMode} setIsDarkMode={setIsDarkMode} />
      <Header isDarkMode={isDarkMode}/>
      <About isDarkMode={isDarkMode}/>
      <Projects isDarkMode={isDarkMode}/>
      <Inspiration isDarkMode={isDarkMode}/>
      <Contact isDarkMode={isDarkMode}/>
      <Footer isDarkMode={isDarkMode}/>
    </>
  );
}
