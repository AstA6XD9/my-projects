import React from "react";
import { serviceData, assets } from "@/assets/assets";
import Image from "next/image";
import { motion } from "framer-motion";

const Projects = ({ isDarkMode }) => {
  return (
    <motion.div 
      initial={{ opacity: 0 }}
      whileInView={{ opacity: 1 }}
      transition={{ duration: 0.4 }}
      viewport={{ once: true }}
      id="projects" 
      className={`w-full px-[12%] py-10 scroll-mt-20 ${isDarkMode ? 'bg-gray-900 text-white' : 'bg-gray-50 text-black'}`}
    >
      <motion.div
        initial={{ y: 20, opacity: 0 }}
        whileInView={{ y: 0, opacity: 1 }}
        transition={{ duration: 0.4 }}
        viewport={{ once: true }}
      >
        <h4 className="text-center mb-2 text-lg font-Ovo">
          Here are my projects
        </h4>
        <h2 className="text-center text-5xl font-Ovo">My projects</h2>
        <p className="text-center max-w-2xl mx-auto mt-5 mb-12 font-Ovo">
          Here are my projects in computer science. I have completed projects in C,
          Ada, Java, Python, and more.
        </p>
      </motion.div>
      
      <motion.div 
        initial={{ y: 20, opacity: 0 }}
        whileInView={{ y: 0, opacity: 1 }}
        transition={{ duration: 0.4, delay: 0.1 }}
        viewport={{ once: true }}
        className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-auto gap-6 my-10"
      >
        {serviceData.map(({ icon, title, description, link }, index) => (
          <motion.div 
            initial={{ y: 20, opacity: 0 }}
            whileInView={{ y: 0, opacity: 1 }}
            transition={{ duration: 0.4, delay: 0.2 + index * 0.1 }}
            viewport={{ once: true }}
            key={index} 
            className={`border border-gray-200 rounded-xl px-8 py-12 shadow-lg hover:shadow-2xl transition-all duration-300 cursor-pointer hover:-translate-y-2 hover:border-blue-300 group ${isDarkMode ? 'bg-gray-800 border-gray-700 hover:bg-gray-700' : 'bg-white hover:bg-gradient-to-br hover:from-blue-50 hover:to-indigo-50'}`}
          >
            <div className="bg-gradient-to-br from-blue-100 to-indigo-100 p-3 rounded-lg w-fit mb-4 group-hover:from-blue-200 group-hover:to-indigo-200 transition-colors duration-300">
              <Image src={icon} alt="" className="w-8 h-8" />
            </div>
            <h3 className={`text-xl font-semibold my-4 group-hover:text-blue-700 transition-colors duration-300 ${isDarkMode ? 'text-white' : 'text-gray-800'}`}>{title}</h3>
            <p className={`text-sm leading-6 mb-4 ${isDarkMode ? 'text-gray-300' : 'text-gray-600'}`}>{description}</p>
            <a href={link} className={`flex items-center gap-2 text-sm font-medium group-hover:translate-x-1 transition-all duration-300 ${isDarkMode ? 'text-blue-400 hover:text-blue-300' : 'text-blue-600 hover:text-blue-800'}`}>
              Read more{" "}
              <Image src={assets.right_arrow} alt="" className="w-4 group-hover:translate-x-1 transition-transform duration-300" />
            </a>
          </motion.div>
        ))}
      </motion.div>
    </motion.div>
  );
};

export default Projects;
